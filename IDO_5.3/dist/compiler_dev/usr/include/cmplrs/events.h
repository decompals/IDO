/*

	EVENTS.H

*/
		/*
		 * This structure is used to gather
		 * information from an events section
		 * record. Since each record is variable
		 * length and is context dependent based
		 * on it's type, the fields for arg[1-3]
		 * are large.
		 * 
		 * It is not expected that this structure
		 * would be used in a table or linked list
		 * thus size is less of a concern.
		 */
typedef struct {
    __uint64_t	fevnt_arg1;	    /* first argument */
    __uint64_t	fevnt_pre_arg1;	    /* raw arg1 value */
    __uint64_t	fevnt_arg2;	    /* first argument */
    __uint64_t	fevnt_pre_arg2;	    /* raw arg1 value */
    __uint64_t	fevnt_arg3;	    /* first argument */
    __uint64_t	fevnt_pre_arg3;	    /* raw arg1 value */
    __uint64_t	fevnt_base;	    /* section base addr */
    __uint32_t	fevnt_type;	    /* record type */
    __uint32_t	fevnt_offset;	    /* accumulated offset */
    __uint32_t	fevnt_index;	    /* text section index */
} Full_Events;

char *event_get_next_rec(
	char *,			/* pointer into section */
	__uint32_t ,		/* current offset into text section */
	Full_Events *);	/* information from record */

__uint32_t event_find_record(
	char *,			/* pointer into the events section */
	Full_Events *,	/* structure for events record info */
	__uint32_t, 		/* event section type */
	char *);		/* end of the current events section */

