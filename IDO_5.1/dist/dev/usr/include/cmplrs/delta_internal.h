#ifndef DELTA_INTERNAL_H
#define DELTA_INTERNAL_H 1

#include "delta_runtime.h"

enum _delta_access {
    _DELTA_ACCESS_PUBLIC,
    _DELTA_ACCESS_PROTECTED,
    _DELTA_ACCESS_PRIVATE,
    _DELTA_ACCESS_UNKNOWN
};

enum _delta_alignment {
    _DELTA_BYTE_ALIGNED = 1,
    _DELTA_HALF_ALIGNED = 2,
    _DELTA_WORD_ALIGNED = 4,
    _DELTA_DOUBLE_ALIGNED = 8
};   

struct _delta_base {
    char is_virtual;
    enum _delta_access access;
    short offset;
    struct __classinfo *base_class;
};

struct _delta_data {
    char is_static;
    enum _delta_access access;
    char *name;
    
    short size;
    short offset;
    short vbase_offset;
    unsigned char bit_field_size;
    unsigned char bit_field_position;

    void *var_ptr;

    void *info;
    struct __classinfo *qualified_class;
    struct __classinfo *declaring_class;
};

struct _delta_method {
    char is_static;
    char is_virtual;
    enum _delta_access access;
    char *name;

    short this_offset;
    short vtable_offset;
    short vptr_offset;
    short vbase_offset;

    void *function_ptr;

    void *info;
    struct __classinfo *qualified_class;
    struct __classinfo *declaring_class;
    struct __classinfo *defining_class;
};  

#endif /* DELTA_INTERNAL_H */
