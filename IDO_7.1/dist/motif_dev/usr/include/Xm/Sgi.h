#ifndef _Sgi_h
#define _Sgi_h

/*****************************************************************************/
/*****************************************************************************/
/**                                                                         **/
/**                          Types for Hash Tables                          **/
/**                                                                         **/
/*****************************************************************************/

/* --- POINTER_TO_* is eq hash.  STRING_TO_* is case-insensitive string --- */
/* --- hash.  *_TO_POINTER means it returns void*, *_TO_INTEGER means   --- */
/* --- it returns int.  NULL and 0 represent the empty key so you can't --- */
/* --- hash on them.						        --- */

enum sgHashTableType {
    HT_POINTER_TO_POINTER,
    HT_STRING_TO_POINTER,
    HT_INTEGER_TO_POINTER,
    HT_POINTER_TO_INTEGER,
    HT_STRING_TO_INTEGER,
    HT_INTEGER_TO_INTEGER
};


typedef int sgTableSizeType;

typedef union _sgKey {
    void *    pointer_key;
    int       integer_key;
} sgKey;

typedef struct _sgHashTableEntry {
    sgKey  key;
    union {
	void *    pointer_val;
	int       integer_val;
    } val;
} sgHashTableEntry, *sgHashTableEntryP;

typedef struct _sgHashTable {
    sgHashTableEntry     *body;
    enum sgHashTableType  type;
    sgTableSizeType       size;
    sgTableSizeType       nentries;  /* must be less than size so that */
                                     /* gethash does't loop forever.   */
    sgTableSizeType       max_entries;  /* (size * REHASH_THRESHOLD) */
}  sgHashTable;


typedef struct _sgHashTable *sgHashTableP;


/*****************************************************************************/
/*****************************************************************************/
/**                                                                         **/
/**                  Prototypes for Hash Table Routines                     **/
/**                                                                         **/
/*****************************************************************************/

#ifndef SKIP_DECLS
#ifdef _NO_PROTO

/* --- Public Function Declarations --- */
extern sgHashTableP  _sgMakeHashTable();
extern void	     _sgFreeHashTable();
extern void	     _sgClearHashTable();
extern void	     _sgRemHashp();
extern void	     _sgRemHashi();
extern void *        _sgGetHashKeyp();

/* --- pointer or string to pointer or string --- */
extern void *      _sgGetHashpp();
extern void	   _sgPutHashpp();
extern void	   _sgMapHashpp();

/* --- pointer or string to int --- */
extern int        _sgGetHashpi();
extern void	  _sgPutHashpi();
extern void	  _sgMapHashpi();

/* --- int to pointer or string --- */
extern void *    _sgGetHaship();
extern void	 _sgPutHaship();
extern void	 _sgMapHaship();

/* --- int to int --- */
extern int        _sgGetHashii();
extern void	  _sgPutHashii();
extern void	  _sgMapHashii();

#else

extern  sgHashTableP  _sgMakeHashTable(enum sgHashTableType);
extern  void          _sgFreeHashTable(sgHashTableP);
extern  void	      _sgClearHashTable(sgHashTableP);
extern  void	      _sgRemHashp(void *, sgHashTableP);
extern  void	      _sgRemHashi(int, sgHashTableP);
extern  void *        _sgGetHashKeyp(void *, sgHashTableP);

/* --- pointer or string to pointer or string --- */
extern  void *       _sgGetHashpp(void *, sgHashTableP);
extern  void	     _sgPutHashpp(void *, sgHashTableP, void *);
extern  void	     _sgMapHashpp(sgHashTableP, void (*)(), int);  

/* --- pointer or string to int --- */
extern  int       _sgGetHashpi(void *, sgHashTableP);
extern  void	  _sgPutHashpi(void *, sgHashTableP, int);
extern  void	  _sgMapHashpi(sgHashTableP, void (*)(), int);

/* --- int to pointer or string --- */
extern  void *       _sgGetHaship(int, sgHashTableP);
extern  void	     _sgPutHaship(int, sgHashTableP, void *);
extern  void	     _sgMapHaship(sgHashTableP, void (*)(), int);

/* --- int to int --- */
extern  int       _sgGetHashii(int, sgHashTableP);
extern  void	  _sgPutHashii(int, sgHashTableP, int);
extern  void	  _sgMapHashii(sgHashTableP, void (*)(), int);
#endif
#endif

#endif /* _Sgi_h */
/* DON'T ADD STUFF AFTER THIS #endif */
