(* file: llvm_bindings.ml *)
open Ctypes
open Foreign

let libLLVM = Dl.dlopen ~filename:"/opt/homebrew/opt/llvm/lib/libLLVM.dylib" ~flags:[]

(******************************************************************************
 * TYPES (Opaque references to LLVM objects)
 ******************************************************************************)
let llcontext = ptr void
let llmodule = ptr void
let llbuilder = ptr void
let lltype = ptr void
let llvalue = ptr void
let llbasicblock = ptr void
let ulonglong = ulong

let llvm_function name fn =
  foreign ~from:libLLVM name fn

(******************************************************************************
 * CREATION / DISPOSAL
 ******************************************************************************)
let create_context =
  llvm_function "LLVMContextCreate" (void @-> returning (ptr void))

let create_module =
  llvm_function "LLVMModuleCreateWithNameInContext"
  (string @-> llcontext @-> returning llmodule)

let create_builder =
  llvm_function "LLVMCreateBuilderInContext" (llcontext @-> returning llbuilder)

let dispose_context =
  llvm_function "LLVMContextDispose" (llcontext @-> returning void)

let dispose_module =
  llvm_function "LLVMDisposeModule" (llmodule @-> returning void)

let dispose_builder =
  llvm_function "LLVMDisposeBuilder" (llbuilder @-> returning void)

(******************************************************************************
 * TYPES
 ******************************************************************************)
let int1_type =
  llvm_function "LLVMInt1TypeInContext" (llcontext @-> returning lltype)

let int8_type =
  llvm_function "LLVMInt8TypeInContext" (llcontext @-> returning lltype)

let int32_type =
  llvm_function "LLVMInt32TypeInContext" (llcontext @-> returning lltype)

let int64_type =
  llvm_function "LLVMInt64TypeInContext" (llcontext @-> returning lltype)

let float_type =
  llvm_function "LLVMFloatTypeInContext" (llcontext @-> returning lltype)

let void_type =
  llvm_function "LLVMVoidTypeInContext" (llcontext @-> returning lltype)

let pointer_type =
  llvm_function "LLVMPointerType" (lltype @-> int @-> returning lltype)

let struct_create_named =
  llvm_function "LLVMStructCreateNamed" (llcontext @-> string @-> returning lltype)

let struct_set_body =
  llvm_function "LLVMStructSetBody" (lltype @-> ptr lltype @-> int @-> bool @-> returning void)

(* LLVMFunctionType(resultType, paramTypes, paramCount, isVarArg) *)
let function_type =
  llvm_function "LLVMFunctionType" (lltype @-> ptr lltype @-> int @-> int @-> returning lltype)

let add_function =
  llvm_function "LLVMAddFunction" (llmodule @-> string @-> lltype @-> returning llvalue)

(* Some convenience for constant creation *)
let const_int =
  llvm_function "LLVMConstInt"
    (lltype @-> ulonglong @-> bool @-> returning llvalue)

let const_real =
  llvm_function "LLVMConstReal"
    (lltype @-> float @-> returning llvalue)

(******************************************************************************
 * BASIC BLOCK
 ******************************************************************************)
let append_basic_block =
  llvm_function "LLVMAppendBasicBlockInContext"
    (llcontext @-> llvalue @-> string @-> returning llbasicblock)

let position_builder_at_end =
  llvm_function "LLVMPositionBuilderAtEnd"
    (llbuilder @-> llbasicblock @-> returning void)

(******************************************************************************
 * INSTRUCTIONS
 ******************************************************************************)
let build_ret_void =
  llvm_function "LLVMBuildRetVoid" (llbuilder @-> returning llvalue)

let build_ret =
  llvm_function "LLVMBuildRet" (llbuilder @-> llvalue @-> returning llvalue)

(* build_call2 was introduced in LLVM 15+; if it doesn't exist, fallback to LLVMBuildCall. *)
let build_call2 =
  llvm_function "LLVMBuildCall2"
    (llbuilder @-> lltype @-> llvalue @-> ptr llvalue @-> int @-> string @-> returning llvalue)

let build_load2 =
  llvm_function "LLVMBuildLoad2"
    (llbuilder @-> lltype @-> llvalue @-> string @-> returning llvalue)

(* alloca: LLVMBuildAlloca(LLVMBuilderRef B, LLVMTypeRef Ty, const char *Name) *)
let build_alloca =
  llvm_function "LLVMBuildAlloca"
    (llbuilder @-> lltype @-> string @-> returning llvalue)

let build_store =
  llvm_function "LLVMBuildStore"
    (llbuilder @-> llvalue @-> llvalue @-> returning llvalue)

let build_gep2 =
  llvm_function "LLVMBuildGEP2"
    (llbuilder @-> lltype @-> llvalue @-> ptr llvalue @-> int @-> string @-> returning llvalue)

(* Arithmetic ops *)
let build_add =
  llvm_function "LLVMBuildAdd" (llbuilder @-> llvalue @-> llvalue @-> string @-> returning llvalue)

let build_sub =
  llvm_function "LLVMBuildSub" (llbuilder @-> llvalue @-> llvalue @-> string @-> returning llvalue)

let build_mul =
  llvm_function "LLVMBuildMul" (llbuilder @-> llvalue @-> llvalue @-> string @-> returning llvalue)

let build_sdiv =
  llvm_function "LLVMBuildSDiv" (llbuilder @-> llvalue @-> llvalue @-> string @-> returning llvalue)

let build_srem =
  llvm_function "LLVMBuildSRem" (llbuilder @-> llvalue @-> llvalue @-> string @-> returning llvalue)

(* Comparisons *)
let llvm_int_pred = uint8_t

let build_icmp =
  llvm_function "LLVMBuildICmp"
    (llbuilder @-> llvm_int_pred @-> llvalue @-> llvalue @-> string @-> returning llvalue)

(* Branching *)
let build_cond_br =
  llvm_function "LLVMBuildCondBr"
    (llbuilder @-> llvalue @-> llbasicblock @-> llbasicblock @-> returning llvalue)

let build_br =
  llvm_function "LLVMBuildBr"
    (llbuilder @-> llbasicblock @-> returning llvalue)

(******************************************************************************
 * CASTS, ETC.
 ******************************************************************************)
let build_bitcast =
  llvm_function "LLVMBuildBitCast"
    (llbuilder @-> llvalue @-> lltype @-> string @-> returning llvalue)

let write_bitcode_to_file =
  llvm_function "LLVMWriteBitcodeToFile"
    (llmodule @-> string @-> returning int)

let simple_test () =
  (* build int main() { return 1 + 1; } to test *)
  let context = create_context () in
  let module_ = create_module "test" context in
  let builder = create_builder context in
  let int32 = int32_type context in
  let args = CArray.make lltype 0 in
  let args_ptr = CArray.start args in
  let main_type = function_type int32 args_ptr 0 0 in
  let main = add_function module_ "main" main_type in
  let entry = append_basic_block context main "entry" in
  position_builder_at_end builder entry;
  let ulong_1 = Unsigned.ULong.of_int 1 in
  let one = const_int int32 ulong_1 false in
  let one_plus_one = build_add builder one one "addtmp" in
  ignore (build_ret builder one_plus_one);
  ignore (write_bitcode_to_file module_ "test.bc");
  ignore (Sys.command "clang test.bc -o scratch_bin");
  dispose_builder builder;
  dispose_module module_;
  dispose_context context
