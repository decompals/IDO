
////////////////////////////////////////////////////////////////////////////////
///////   Copyright 1992, Silicon Graphics, Inc.  All Rights Reserved.   ///////
//                                                                            //
// This is UNPUBLISHED PROPRIETARY SOURCE CODE of Silicon Graphics, Inc.;     //
// the contents of this file may not be disclosed to third parties, copied    //
// or duplicated in any form, in whole or in part, without the prior written  //
// permission of Silicon Graphics, Inc.                                       //
//                                                                            //
// RESTRICTED RIGHTS LEGEND:                                                  //
// Use,duplication or disclosure by the Government is subject to restrictions //
// as set forth in subdivision (c)(1)(ii) of the Rights in Technical Data     //
// and Computer Software clause at DFARS 252.227-7013, and/or in similar or   //
// successor clauses in the FAR, DOD or NASA FAR Supplement. Unpublished -    //
// rights reserved under the Copyright Laws of the United States.             //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

//////////////////////////////////////////////////////////////////////////////
//
//	VkNameList Class Definition 
//
//
//////////////////////////////////////////////////////////////////////////////

#ifndef VKNAMELIST_H
#define VKNAMELIST_H

#include <Xm/Xm.h>

void FreeIdTable (char**);

class VkNameList {

    public:

	VkNameList ();
	VkNameList (const VkNameList&);
	VkNameList (char*);
	~VkNameList ();

	void clear	();
	void add	(char*);	// dups input
	void add	(const VkNameList&);	// dups input
	void remove	(char*);

	void sort	();		// uses qsort
        void reverse	();
	void removeDuplicates ();

	int  size   () const { return _size; }
	int  exists (char*) const;	// returns 1 if char* matches atleast
					// 1 item in the list.

	VkNameList&  operator=	(const VkNameList&);
	char*	   operator[]	(int index)	const;
	VkNameList   operator[]	(char*)		const;
	int	   operator==	(const VkNameList&) const;

	char*	 mostCommonString () const;

	virtual VkNameList* completeName (char* name, char*& completedName, int& nMatching);
	// attempt to complete <name>
	//
	//------------------------------------------------------------------------
	// condn.		return val	completedName 		nMatching
	//------------------------------------------------------------------------
	// no match		null list	longest sub-string	  0
	//					matching at least one 
	//------------------------------------------------------------------------
	// more than one	all those	most common string	  no. of
	// match		who match	among the matched	macthes
	//
	//------------------------------------------------------------------------
	// exactly one match	matched		copy of matched		  1
	//			(completed)	string
	//			string
	//------------------------------------------------------------------------

	operator char** 	() const;
	operator XmStringTable	() const;

    private:

	char**	_names;
	int	_size;

};

#endif

