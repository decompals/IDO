typedef int (*__runtime_vptp)();

struct __runtime_mptr {
    long d;
    long i;
    __runtime_vptp f;
};

#define DELTA_INIT_OFFSET_TO_D_FIELD 0
#define DELTA_INIT_OFFSET_TO_F_FIELD 8

struct __runtime_vtblinfo {
    short instance_offset;
    short vtable_offset;
    short vbase_offset;
};

struct __runtime_vbaseinfo {
    short vbase_offset;
    short vbase_ptr_offset;
    struct __runtime_classinfo *vbase_info;
};

struct __runtime_classinfo {
    struct __runtime_mptr *vtable;
    struct __runtime_vtblinfo *vtbl_info;
    struct __runtime_vbaseinfo *vbase_info;
    struct __runtime_vbaseinfo *vbase_info_end;
    __runtime_vptp ctor;
    __runtime_vptp dtor;
    __runtime_vptp cctor;
};

