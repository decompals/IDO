#ifndef __RUNTIME_H__
#define __RUNTIME_H__
#ifdef __cplusplus
extern "C" {
#endif

#ifdef DEBUG
typedef char *__dynamic_vptp;
#else
typedef int (*__dynamic_vptp)();
#endif

struct delta_hash_table_entry {
    struct delta_hash_table_entry *next;
    char *key;
    void *data;
};

struct delta_hash_table{
    int size;
    struct delta_hash_table_entry tbl[1];    /* This must be at the end of the structure */
};

extern struct delta_hash_table *delta_hashinit(unsigned int size);
extern unsigned int delta_hashmod(struct delta_hash_table *phashtab, char *pname);
extern int delta_hashlookup(struct delta_hash_table *phashtab, char *name, void **val);
extern void *delta_hashenter(struct delta_hash_table *phashtab, char *name, void *data);
extern void delta_deletehashtable(struct delta_hash_table *phashtab);

struct __dynamic_mptr {
    short d;
    short i;
    __dynamic_vptp f;
    struct __specclassinfo *info;
    struct __dynamic_mptr *virtvtbl;
};

#define INVALID_VO_SIZE 1
#define INVALID_MO_SIZE 2
#define INVALID_SVO_SIZE 4
#define INVALID_BASE_CLASSES 8
#define INVALID_INTERNAL_ERROR 16
#define INVALID_UNDEFINED_CLASS 32

#define MAX_VO_SIZE 0x8000
#define MAX_MO_SIZE 0x1000
#define MAX_SVO_SIZE 0x8000

/* values for method_type field in __moinfo */

#define __RUNTIME_PURE_VIRTUAL 3
#define __RUNTIME_STATIC 2
#define __RUNTIME_VIRTUAL 1
#define __RUNTIME_NONVIRTUAL 0

/* values for protection fields */

#define __RUNTIME_PUBLIC 2
#define __RUNTIME_PROTECTED 1
#define __RUNTIME_PRIVATE 0

/* values for internal_type field in __voinfo */

#define __RUNTIME_REG 0
#define __RUNTIME_VPTR 1
#define __RUNTIME_VCLASS_PTR 2

/* values for virt field in __coinfo */

#define __RUNTIME_PARENT_CLASS 0
#define __RUNTIME_VIRTUAL_CLASS 1
#define __RUNTIME_ANCESTOR_CLASS 2
#define __RUNTIME_THIS_CLASS 3

/* values for can_switch_qualifying_class */

#define __RUNTIME_FIXED_QUAL 0
#define __RUNTIME_CHANGE_QUAL 1
#define __RUNTIME_NO_QUAL 2

struct __moinfo {
    char *name;
    short mo;
    short so;
    __dynamic_vptp f;
    short d;
    char method_type;
    char protection;
    char internally_generated;
    char can_switch_qualifying_class;
    char first;
    char defined;
    char *return_signature;
    struct __classinfo *defining_class;
    struct __classinfo *declaring_class;
    struct __classinfo *qualifying_class;
    struct __classinfo *info;
    long next;
    short vptr_offset;
};

struct __vmoinfo {
    char *name;
    __dynamic_vptp f;
};

struct __voinfo {
    char *name;
    short vo;
    short sz;
    char alignment; /* 0 byte, 1 halfword, 2 word, 3 double word */
    signed char bitfield_size; /* -1 indicates no bitfield, 0 realign bit field */ 
    char bitfield_pos;
    short entry_index; /* index used for anonymous unions */
    char *class_name;
    char defined;	/* for defined entries this is indicates whether this field is defined in the current class */
                        /* for vo_table entries it is the depth to the root of the class tree */
    char internal_type;
    char protection;
    char can_switch_qualifying_class;
    char first;
    char *type_signature;
    struct __classinfo *defining_class;
    struct __classinfo *qualifying_class;
    struct __virtualbaseinfo *vbase_class;
    long next;
};

struct __svoinfo {
    char *name;
    short svo;
    char protection;
    char can_switch_qualifying_class;
    char first;
    char *var;	    /* has a value if defined in class */
    char *type_signature;
    struct __classinfo *defining_class;
    struct __classinfo *qualifying_class;
    long next;
};

struct __coinfo {
    char *name;
    short mo;
    short vtbl_mo;
    short so;
    short sz;
    char alignment;
    char virt;
    char protection;
    struct __classinfo *info;
};

struct __vtableinfo {
    short vo;
    short mo;
    short co;
};

struct __selfvbaseinfo {
    short vo;
    short so;
};

struct __virtualinfo {
    struct __classinfo *info;
    short first;
};

struct __virtualbaseinfo {
    char *name;
    short vo_offset;	    /* instance position of defining class */
    short mo_offset;	    /* method poistion of defining class */
    short vtbl_mo_offset;	    /* method poistion of defining class */
    short svo_offset;
    short vo_vb_offset;	    /* position of instance virtual ptr */
    short virtual_vo_offset;	/* instance position of virtual class */
    short virtual_mo_offset;	/* method position of virtual class */
    short virtual_vtbl_mo_offset;	/* method position of virtual class */
    short virtual_svo_offset;
    short virtual_vo_table_offset;
    short virtual_entry_index;
    struct __moinfo *mo_info;
    struct __svoinfo *svo_info;
    long next;
    struct __classinfo *info;
    char first;
    char isBase;	    /* non-zero if base offset, 2 if brand new offset */
    short sz;
    char alignment;
};

struct __classinfo {
    char *name;
    char initialized;
    char stored;
    char invalid;
    char nondyn;

    short num_defined_mo_entries;
    short num_defined_vmo_entries;
    short num_defined_vo_entries;
    short num_defined_svo_entries;
    short num_defined_co_entries;
    short num_defined_virtual_co_entries;

    struct __moinfo **defined_mo_table;
    struct __vmoinfo *defined_vmo_table;
    struct __voinfo **defined_vo_table;
    struct __svoinfo **defined_svo_table;
    struct __coinfo **defined_co_table;

    unsigned short *class_depth;
    char alignment;

    void *(*nw)(unsigned int);
    __dynamic_vptp ctor;
    __dynamic_vptp ctor2;
    __dynamic_vptp dtor;
    __dynamic_vptp as;

    short mo_size;
    short full_mo_size;
    short max_mo_size;
    short vtbl_mo_size;
    short full_vtbl_mo_size;
    short max_vtbl_mo_size;
    struct __moinfo *mo_table;

    short vo_size;
    short full_vo_size;
    short vo_table_size;
    short full_vo_table_size;
    short max_vo_table_size;
    short remaining_bits;
    short entry_index;
    short full_entry_index;
    struct __voinfo *vo_table;

    short svo_size;
    short full_svo_size;
    short max_svo_size;
    struct __svoinfo *svo_table;

    short vt_size;
    short full_vt_size;
    short max_vt_size;
    struct __vtableinfo *vtable_info;

    short self_vb_size;
    short full_self_vb_size;
    short max_self_vb_size;
    struct __selfvbaseinfo *self_base_table;

    short bc_size;
    short max_bc_size;
    struct __coinfo *co_table;

    short virtual_base_size;
    struct __virtualinfo *virtual_info;

    short vb_size;
    short max_vb_size;
    struct __virtualbaseinfo *vb_info;

    short vptr_offset;
    short self_vbaseptr_offset;

    struct delta_hash_table *mo_hash;
    struct delta_hash_table *vo_hash;
    struct delta_hash_table *unknown_svo;
      
    void *classSymbol;	/* Hook for C++/WorkShop to store a SymbolPtr */
};

#define __LDX_DYNAMICS 0
#define __LDX_NO_DYNAMICS 1
#define __LDX_READ_ERROR 2

extern void __initialize_delta_runtime(int can_free);
extern void __reset_delta_runtime(void);

extern int __extract_classinfo_from_file_descriptor(int fildes);
extern int __extract_classinfo(char *filename);

extern struct __classinfo *__get_class_information(const char *name, int init);
extern int __store_class_information(struct __classinfo *info);
extern void __initialize_class(struct __classinfo *info);
extern int __align_data(int currentSize, char alignment);
extern void __initialize_all_stored_classes(void);

extern struct __classinfo *__class_iterate(int (*func)(struct __classinfo *, void *), void *data);

extern void __ldx_free_all_class_info(void);
#ifdef DEBUG
extern void __print_class_information(struct __classinfo *info);
#endif

void __set_callback_function(struct __classinfo *(*rtn)(const char *, void *), void *data);

extern struct __classinfo *drt_begin_class_description(const char *name); /* returns classinfo if class already entered , NULL otherwise */
extern struct __classinfo *drt_end_class_description(void);
extern void drt_enter_base_class(const char *name, char virt, char protection);
extern void drt_enter_method(const char *name, __dynamic_vptp f, char method_type, char protection, char *return_type);
extern void drt_enter_member(const char *name, long sz, long alignment, const char *class_name, char defined, char internal_type, char protection, char *type);
extern void drt_enter_bitfield_member(const char *name, long sz, long alignment, long bitfield_size, char defined, char internal_type, char protection, char *type);
extern void drt_enter_static_member(const char *name, long sz, char *var, char protection, char *type);
extern void drt_enter_static_base_class(const char *name, char virt, char protection, long sz, char alignament);

extern void drt_enter_method_no_copy(const char *name, __dynamic_vptp f, char method_type, char protection, void *return_type);
extern void drt_enter_member_no_copy(const char *name, long sz, long alignment, const char *class_name, char defined, char internal_type, char protection, void *type);
extern void drt_enter_bitfield_member_no_copy(const char *name, long sz, long alignment, long bitfield_size, char defined, char internal_type, char protection, void *type);
extern void drt_enter_static_member_no_copy(const char *name, long sz, void *var, char protection, void *type);

extern struct __classinfo *drt_begin_nondyn_class_description(const char *name,
						      long size,
						      long alignment,
						      long vtable_size);
extern void drt_enter_nondyn_base_class(const char *name, long pos, long vtbl_pos, char virt, char protection);
extern void drt_enter_nondyn_method_no_copy(const char *name, __dynamic_vptp f, char method_type, int vtbl_pos, char protection, void *return_type);
extern void drt_enter_nondyn_method(const char *name, __dynamic_vptp f, char method_type, int vtbl_pos, char protection, char *return_type);
extern void drt_enter_nondyn_member_no_copy(const char *name,
				      long pos,
				      long sz,
				      long alignment,
				      char protection,
				      void *type);
extern void drt_enter_nondyn_member(const char *name,
				      long pos,
				      long sz,
				      long alignment,
				      char protection,
				      void *type);
extern void drt_enter_nondyn_bitfield_member_no_copy(const char *name,
					       long pos,
					       long sz,
					       long alignment,
					       long bitfield_pos,
					       long bitfield_size,
					       char protection,
					       void *type);
extern void drt_enter_nondyn_bitfield_member(const char *name,
				       long pos,
				       long sz,
				       long alignment,
				       long bitfield_pos,
				       long bitfield_size,
				       char protection,
				       void *type);
extern void drt_enter_nondyn_vptr_no_copy(int pos, void *type);
extern void drt_enter_nondyn_vptr(int pos, void *type);


struct __object_info {
    void *ptr;
    long offset;
    long vbaseptr_offset;
    struct __classinfo *info;
};

struct __method_info {
    void *calling_ptr;
    long calling_offset;
    void *passing_ptr;
    long passing_offset;
    long vptr_offset;
    long vbaseptr_offset;
    __dynamic_vptp f;
    struct __moinfo *method;
};

struct __instance_info {
    void *ptr;
    long offset;
    long vbaseptr_offset;
    struct __voinfo *instance;
    int ambig;
};

struct __static_instance_info {
    void *ptr;
    struct __svoinfo *instance;
    int ambig;
};

struct _delta_catalog_malloc_pool_item {
    char *pool;
    int remaining;
    int size;
    struct _delta_catalog_malloc_pool_item *next;
};

struct _delta_catalog {
    struct __classinfo **class_info_table;
    long num_of_classes;
    long max_num_of_classes;
    struct _delta_catalog **library_catalogs;
    long num_of_library_catalogs;
    long max_num_of_library_catalogs;
    struct __classinfo *(*fill_in_routine)(const char *, void *);
    void *fill_in_data;
    int (*missing_class_error_proc)(const char *, struct __classinfo *, void *);
    void *missing_class_error_data;
    void *info;
    struct _delta_catalog_malloc_pool_item *pool;
};

extern struct _delta_catalog_malloc_pool_item *delta_int_get_pool_item(int size);

extern struct _delta_catalog *current_catalog;

#ifdef __cplusplus
}
#endif
#endif /* __RUNTIME_H__ */
