Value* require(ValueTag type, Value* x) {
  Value* v = NULL;
  if (x->type != type) {
    if (x->type == vERROR) {
      v = x;
    } else {
      v = vError(typeError(type, x->type));
    }
  }
  return v;
}

Value* apply1(Value* f, Value* arg) {
  Value* v = NULL;
  v = require(vFUN, f);
  if (v == NULL) {
    switch (f->primFun.type) {
    case PRIMFUN1:
      v = f->primFun.f1(arg);
      break;
    case PRIMFUN2:
      if (f->primFun.f2.arg1 == NULL) {
        v = ap1PrimFun2(f->primFun.f2, arg);
      } else {
        v = f->primFun.f2.fun2(f->primFun.f2.arg1, arg);
      }
      break;
    case PRIMFUN3:
      if (f->primFun.f3.arg1 == NULL && f->primFun.f3.arg2 == NULL) {
        v = ap1PrimFun3(f->primFun.f3, arg);
      } else if (f->primFun.f3.arg1 != NULL && f->primFun.f3.arg2 == NULL) {
        v = ap2PrimFun3(f->primFun.f3, f->primFun.f3.arg1, arg);
      } else if (f->primFun.f3.arg1 != NULL && f->primFun.f3.arg2 != NULL) {
        v = f->primFun.f3.fun3(f->primFun.f3.arg1, f->primFun.f3.arg2, arg);
      } else {
        int UNEXPECTED_PRIMFUN3_STATE = 0;
        assert(UNEXPECTED_PRIMFUN3_STATE);
      }
      break;
    default:
      int UNHANDLED_PRIMFUN_TAG = 0;
      assert(UNHANDLED_PRIMFUN_TAG);
    }
  }
  return v;
}
