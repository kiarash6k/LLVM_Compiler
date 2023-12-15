#ifndef COMP_CODEGEN_CODEGENERATOR_H
#define COMP_CODEGEN_CODEGENERATOR_H

#include "comp/AST/AST.h"
#include "llvm/Target/TargetMachine.h"
#include <string>

namespace comp {

class CodeGenerator {
  llvm::LLVMContext &Ctx;
  llvm::TargetMachine *TM;
  ModuleDeclaration *CM;

protected:
  CodeGenerator(llvm::LLVMContext &Ctx, llvm::TargetMachine *TM)
      : Ctx(Ctx), TM(TM), CM(nullptr) {}

public:
  static CodeGenerator *create(llvm::LLVMContext &Ctx, llvm::TargetMachine *TM);

  std::unique_ptr<llvm::Module> run(ModuleDeclaration *CM, std::string FileName);
};
} // namespace comp
#endif