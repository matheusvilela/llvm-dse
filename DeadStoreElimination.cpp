#include "DeadStoreElimination.h"
#include "llvm/Support/CallSite.h"

using namespace llvm;

static RegisterPass<DeadStoreEliminationPass>
X("dead-store-elimination", "Remove dead stores", false, true);

static uint64_t getPointerSize(const Value *V, AliasAnalysis &AA) {
  uint64_t Size;
  if (getObjectSize(V, Size, AA.getDataLayout(), AA.getTargetLibraryInfo()))
    return Size;
  else {
    return AA.getTypeStoreSize(V->getType());
  }
}

void DeadStoreEliminationPass::getAnalysisUsage(AnalysisUsage &AU) const {
  AU.addRequired<AliasAnalysis>();
  AU.addRequired<MemoryDependenceAnalysis>();
  AU.setPreservesAll();
}

DeadStoreEliminationPass::DeadStoreEliminationPass() : ModulePass(ID) {
  RemovedStores   = 0;
  FunctionsCount  = 0;
  FunctionsCloned = 0;
  ClonesCount     = 0;
  CallsCount      = 0;
  PromissorCalls  = 0; //FIXME: get this stats
  CallsReplaced   = 0;
}
DeadStoreEliminationPass::~DeadStoreEliminationPass() {
  delete globalsAST;
}

bool DeadStoreEliminationPass::changeLinkageTypes(Module &M) {
  DEBUG(errs() << "Changing linkages to private...\n");
  for (Module::global_iterator git = M.global_begin(), gitE = M.global_end();
        git != gitE; ++git) {
    DEBUG(errs() << "  " << *git << "\n");
    git->setLinkage(GlobalValue::PrivateLinkage);
  }
  for (Module::iterator F = M.begin(), E = M.end(); F != E; ++F) {
    if (!F->isDeclaration()) {
      F->setLinkage(GlobalValue::PrivateLinkage);
      DEBUG(errs() << "  " << F->getName() << "\n");
    }
  }
  DEBUG(errs() << "\n");
  return true;
}

bool DeadStoreEliminationPass::runOnModule(Module &M) {

  if (!getFnThatStoreOnArgs(M)) {
    return false;
  }

  bool changed = false;

  changed    = changed | changeLinkageTypes(M);
  AA         = &getAnalysis<AliasAnalysis>();
  globalsAST = new AliasSetTracker(*AA);

  // Analyse program
  getGlobalValuesInfo(M);
  runOverwrittenDeadStoreAnalysis(M);
  runNotUsedDeadStoreAnalysis();

  // Create clones
  changed = changed | cloneFunctions();
  return changed;
}

/*
 * Create an AST containing all global values that have pointer type
 */
void DeadStoreEliminationPass::getGlobalValuesInfo(Module &M) {
  for (Module::global_iterator git = M.global_begin(), gitE = M.global_end();
        git != gitE; ++git) {
    Value* v = git;
    if (v->getType()->isPointerTy()) {
      globalsAST->add(v, getPointerSize(v, *AA), NULL);
    }
  }
}

/*
 * Build information about functions that store on pointer arguments
 * For simplification, we only consider a function to store on an argument
 * if it has exactly one StoreInst to that argument and the arg has no other use.
 */
int DeadStoreEliminationPass::getFnThatStoreOnArgs(Module &M) {
  int numStores = 0;
  DEBUG(errs() << "Getting functions that store on arguments...\n");
  for (Module::iterator F = M.begin(); F != M.end(); ++F) {
    if (F->arg_empty() || F->isDeclaration()) continue;

    // Get args
    std::set<Value*> args;
    for (Function::arg_iterator formalArgIter = F->arg_begin();
          formalArgIter != F->arg_end(); ++formalArgIter) {
      Value *formalArg = formalArgIter;
      if (formalArg->getType()->isPointerTy()) {
        args.insert(formalArg);
      }
    }

    // Find stores on arguments
    for (Function::iterator BB = F->begin(); BB != F->end(); ++BB) {
      for (BasicBlock::iterator I = BB->begin(); I != BB->end(); ++I) {
        Instruction *inst = I;
        if (!isa<StoreInst>(inst)) continue;
        StoreInst *SI = dyn_cast<StoreInst>(inst);
        Value *ptrOp = SI->getPointerOperand();

        if (args.count(ptrOp) && ptrOp->hasNUses(1)) {
          fnThatStoreOnArgs[F].insert(ptrOp);
          numStores++;
          DEBUG(errs() << "  " << F->getName() << " stores on argument "
                << ptrOp->getName() << "\n"); }
      }
    }
  }
  DEBUG(errs() << "\n");
  return numStores;
}

/*
 * Find stores to arguments that are not read on the caller function. If the
 * corresponding actual argument is locally declared on the caller, the
 * store can be removed with cloning.
 */
void DeadStoreEliminationPass::runNotUsedDeadStoreAnalysis() {

  DEBUG(errs() << "Running not used dead store analysis...\n");
  for(std::map<Function*, std::set<Value*> >::iterator it =
        fnThatStoreOnArgs.begin(); it != fnThatStoreOnArgs.end(); ++it) {
    Function* F = it->first;
    DEBUG(errs() << "  Verifying function " << F->getName() << ".\n");

    // Verify each callsite of functions that store on arguments
    for (Value::use_iterator UI = F->use_begin(), E = F->use_end();
          UI != E; ++UI) {
       User *U = *UI;

      if (isa<BlockAddress>(U)) continue;
      if (!isa<CallInst>(U) && !isa<InvokeInst>(U)) continue;

      Instruction* inst = cast<Instruction>(U);
      if (deadArguments.count(inst)) continue;

      CallSite CS(inst);
      if (!CS.isCallee(UI)) continue;

      CallSite::arg_iterator actualArgIter = CS.arg_begin();
      Function::arg_iterator formalArgIter = F->arg_begin();
      int size = F->arg_size();

      std::set<Value*> storedArgs = fnThatStoreOnArgs[F];
      for (int i = 0; i < size; ++i, ++actualArgIter, ++formalArgIter) {
        Value *formalArg = formalArgIter;
        Value *actualArg = *actualArgIter;

        // Find out if this store may be read within the caller
        if (storedArgs.count(formalArg)) {
          DEBUG(errs() << "    Store on " << formalArg->getName()
                << " may be removed with cloning on instruction " << *inst << "\n");
          if (!isa<AllocaInst>(actualArg)) {
            DEBUG(errs() << "    Can't remove because actual arg was not locally allocated.\n");
            //FIXME: handle malloc and other allocation functions
            continue;
          }
          if (aliasExternalValues(actualArg, *inst->getParent()->getParent())) {
            DEBUG(errs() << "    Can't remove because actual arg alias globals or args.\n");
            continue;
          }
          DEBUG(errs() << "    Can I remove it?\n");
          //TODO: verify if there are any uses of the returned value
        }
      }
    }
  }
  DEBUG(errs() << "\n");
}

/*
 * Verify if a given value alias the arguments of the function where it's
 * used or global values.
 */
bool DeadStoreEliminationPass::aliasExternalValues(Value *v, Function &F) {

  // Verify if value alias globals
  AliasSetTracker* ast = new AliasSetTracker(*AA);
  ast->add(*globalsAST);
  bool aliasGlobals = !ast->add(v, getPointerSize(v, *AA), NULL);
  DEBUG(errs() << "    Global+value AST:\n");
  DEBUG(printSet(errs(), *ast));
  delete ast;

  // Make AliasSetTracker with pointer arguments
  AliasSetTracker* argAST = new AliasSetTracker(*AA);
  for (Function::arg_iterator formalArgIter = F.arg_begin(); formalArgIter !=
      F.arg_end(); ++formalArgIter) {
    Value *formalArg = formalArgIter;
    if (formalArg->getType()->isPointerTy()) {
      argAST->add(formalArg, getPointerSize(formalArg, *AA), NULL);
    }
  }

  // Verify if value alias args
  ast = new AliasSetTracker(*AA);
  ast->add(*argAST);
  DEBUG(errs() << "    Adding value " << *v << ", with size " << getPointerSize(v, *AA) << "\n");
  bool aliasArgs = !ast->add(v, getPointerSize(v, *AA), NULL);
  DEBUG(errs() << "    Arguments+value AST:\n");
  DEBUG(printSet(errs(), *ast));
  delete ast;
  delete argAST;

  if (aliasArgs) DEBUG(errs() << "    Value " << *v << " cannot be removed due to alias args.\n");
  if (aliasGlobals) DEBUG(errs() << "    Value " << *v << " cannot be removed due to alias globals.\n");

  return (aliasArgs || aliasGlobals);
}

/*
 * Find stores to arguments that are overwritten before being read.
 */
void DeadStoreEliminationPass::runOverwrittenDeadStoreAnalysis(Module &M) {
  DEBUG(errs() << "Running overwritten dead store analysis...\n");
  for (Module::iterator F = M.begin(), E = M.end(); F != E; ++F) {
    if (!F->isDeclaration()) {
      FunctionsCount++;
      CallsCount += F->getNumUses();
      runOverwrittenDeadStoreAnalysisOnFn(*F);
    }
  }
  DEBUG(errs() << "\n");
}
void DeadStoreEliminationPass::runOverwrittenDeadStoreAnalysisOnFn(Function &F) {
  MDA       = &getAnalysis<MemoryDependenceAnalysis>(F);

  for (Function::iterator BB = F.begin(), E = F.end(); BB != E; ++BB) {
    for (BasicBlock::iterator I = BB->begin(), IE = BB->end(); I != IE; ++I) {
      Instruction *inst = I;
      if (StoreInst* SI = dyn_cast<StoreInst>(inst)) {
        Value *ptr           = SI->getPointerOperand();
        MemDepResult mdr     = MDA->getDependency(inst);
        Instruction *depInst = mdr.getInst();
        if (depInst && (isa<CallInst>(depInst) || isa<InvokeInst>(depInst))) {
           Function *calledFn;

           if (CallInst* CI = dyn_cast<CallInst>(depInst)) {
             calledFn = CI->getCalledFunction();
           } else {
             InvokeInst *II = dyn_cast<InvokeInst>(depInst);
             calledFn = II->getCalledFunction();
           }
           if (!fnThatStoreOnArgs.count(calledFn)) continue;

           CallSite CS(depInst);

           CallSite::arg_iterator actualArgIter = CS.arg_begin();
           Function::arg_iterator formalArgIter = calledFn->arg_begin();
           int size = calledFn->arg_size();

           std::set<Value*> storedArgs = fnThatStoreOnArgs[calledFn];
           for (int i = 0; i < size; ++i, ++actualArgIter, ++formalArgIter) {
             Value *formalArg = formalArgIter;
             Value *actualArg = *actualArgIter;
             //FIXME: be sure that the store on the caller fully overwrites the
             //store on the callee
             if (ptr == actualArg && storedArgs.count(formalArg)) {
               DEBUG(errs() << "  Store on " << formalArg->getName() << " should be removed with cloning\n");
               deadArguments[depInst].insert(formalArg);
             }
           }
           if (deadArguments.count(depInst)) {
             fn2Clone[calledFn].push_back(depInst);
           }
        }
      }
    }
  }
}

/*
 * Clone functions, removing dead stores
 */
bool DeadStoreEliminationPass::cloneFunctions() {
  bool modified = false;
  for (std::map<Function*, std::vector<Instruction*> >::iterator it =
      fn2Clone.begin(); it != fn2Clone.end(); ++it) {

    Function *F = it->first;
    std::vector<Instruction*> callSitesToClone = it->second;
    std::map< std::set<Value*> , Function*> clonedFns;
    int i = 0;
    FunctionsCloned++;
    for (std::vector<Instruction*>::iterator it2 = callSitesToClone.begin();
        it2 != callSitesToClone.end(); ++it2, ++i) {

      Instruction* caller = *it2;
      std::set<Value*> deadArgs = deadArguments[caller];

      if (!clonedFns.count(deadArgs)) {
        // Clone function if a proper clone doesnt already exist
        std::stringstream suffix;
        suffix << ".deadstores" << i;
        Function* NF = cloneFunctionWithoutDeadStore(F, caller, suffix.str());
        replaceCallingInst(caller, NF);
        clonedFns[deadArgs] = NF;
        ClonesCount++;
      } else {
        // Use existing clone
        Function* NF = clonedFns.at(deadArgs);
        replaceCallingInst(caller, NF);
      }
      CallsReplaced++;
      modified = true;
    }
  }
  return modified;
}

/*
 * Clone a given function removing dead stores
 */
Function* DeadStoreEliminationPass::cloneFunctionWithoutDeadStore(Function *Fn,
    Instruction* caller, std::string suffix) {

  Function *NF = Function::Create(Fn->getFunctionType(), Fn->getLinkage());
  NF->copyAttributesFrom(Fn);

  // Copy the parameter names, to ease function inspection afterwards.
  Function::arg_iterator NFArg = NF->arg_begin();
  for (Function::arg_iterator Arg = Fn->arg_begin(), ArgEnd = Fn->arg_end();
      Arg != ArgEnd; ++Arg, ++NFArg) {
    NFArg->setName(Arg->getName());
  }

  // To avoid name collision, we should select another name.
  NF->setName(Fn->getName() + suffix);

  // Fill clone content
  ValueToValueMapTy VMap;
  SmallVector<ReturnInst*, 8> Returns;
  Function::arg_iterator NI = NF->arg_begin();
  for (Function::arg_iterator I = Fn->arg_begin();
      NI != NF->arg_end(); ++I, ++NI) {
    VMap[I] = NI;
  }
  CloneAndPruneFunctionInto(NF, Fn, VMap, false, Returns);

  // Remove dead stores
  std::set<Value*> deadArgs = deadArguments[caller];
  std::set<Value*> removeStoresTo;
  Function::arg_iterator NFArgIter = NF->arg_begin();
  for (Function::arg_iterator FnArgIter = Fn->arg_begin(); FnArgIter !=
      Fn->arg_end(); ++FnArgIter, ++NFArgIter) {
    Value *FnArg = FnArgIter;
    if (deadArgs.count(FnArg)) {
      removeStoresTo.insert(NFArgIter);
    }
  }
  std::vector<Instruction*> toRemove;
  for (Function::iterator BB = NF->begin(); BB != NF->end(); ++BB) {
    for (BasicBlock::iterator I = BB->begin(); I != BB->end(); ++I) {
      Instruction *inst = I;
      if (!isa<StoreInst>(inst)) continue;
      StoreInst *SI = dyn_cast<StoreInst>(inst);
      Value *ptrOp = SI->getPointerOperand();
      if (removeStoresTo.count(ptrOp)) {
        DEBUG(errs() << "will remove this store: " << *inst << "\n");
        toRemove.push_back(inst);
      }
    }
  }
  for (std::vector<Instruction*>::iterator it = toRemove.begin();
      it != toRemove.end(); ++it) {
    Instruction* inst = *it;
    inst->eraseFromParent();
    RemovedStores++;
  }

  // Insert the clone function before the original
  Fn->getParent()->getFunctionList().insert(Fn, NF);

  return NF;
}

/*
 * Replace called function of a given call site.
 */
void DeadStoreEliminationPass::replaceCallingInst(Instruction* caller,
    Function* fn) {
  if (isa<CallInst>(caller)) {
    CallInst *callInst = dyn_cast<CallInst>(caller);
    callInst->setCalledFunction(fn);
  } else if (isa<InvokeInst>(caller)) {
    InvokeInst *invokeInst = dyn_cast<InvokeInst>(caller);
    invokeInst->setCalledFunction(fn);
  }
}


void DeadStoreEliminationPass::printSet(raw_ostream &O,
    AliasSetTracker &myset) const {
  O << "    {\n";
  for (AliasSetTracker::const_iterator it = myset.begin();
      it != myset.end(); ++it) {
    O << "    ";
    (*it).print(O);
  }
  O << "    }\n";
}

void DeadStoreEliminationPass::print(raw_ostream &O, const Module *M) const {
  O << "Number of dead stores removed: " << RemovedStores << "\n";
}
