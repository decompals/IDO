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
////////////////////////////////////////////////////////
// Data.c++
///////////////////////////////////////////////////////
//
// This object stores all the database of phonebook entries.  Since there is
// only one database, the global theData points to this object.  The entries
// are stored in an alphabetically-sorted linked list.
//

#include <stdio.h>
#include <string.h>
#include <malloc.h>
#include "Data.h"

Data *theData = NULL;

Data::Data()
{
  // Create an empty linked list with a dummy first element

    head = new DataList;
    head->next = NULL;
    dirty = False;

    theData = this;
}

Data::~Data()
{
    deleteData();
    delete head;
}

// Load our database from a file.  The database format is simple ASCII.
// The first line is a version number, so previous formats might be
// automatically upgraded.  Following lines are the entries, four lines
// per entry.  The line ordering is name/phone/address/comment.

Boolean Data::openFile(char *filename)
{
    FILE *fd;
    char version[1024], name[1024], phone[1024], addr[1024], comment[1024];

    fd = fopen(filename, "r");
    if (!fd) 
	return False;

    if (!fgets(version, sizeof(version), fd) ||
	strcmp(version, "1.0\n"))
    {
	fclose(fd);
	return False;
    }
    
    deleteData();

    while (fgets(name, sizeof(name), fd) &&
	   fgets(phone, sizeof(phone), fd) &&
	   fgets(addr, sizeof(addr), fd) &&
	   fgets(comment, sizeof(comment), fd))
    {
	name[strlen(name)-1] = '\0';
	phone[strlen(phone)-1] = '\0';
	addr[strlen(addr)-1] = '\0';
	comment[strlen(comment)-1] = '\0';
	addEntry(name, phone, addr, comment);
    }
    
    fclose(fd);
    dirty = False;
    return (True);
}

// Write out the database to a file.  First write to a temporary file,
// and then rename the temporary file to the real filename.

Boolean Data::writeFile(char *filename)
{
    FILE *fd;
    char temp[1024];
    DataList *ptr;

    sprintf(temp, "%s-XXXXXX", filename);
    mktemp(temp);
    fd = fopen(temp, "w");

    if (!fd) 
	return (False);

    fprintf(fd, "1.0\n");
    ptr = head->next;
    while (ptr)
    {
	fputs(ptr->element->name, fd);
	fputs("\n", fd);
	fputs(ptr->element->phone, fd);
	fputs("\n", fd);
	fputs(ptr->element->addr, fd);
	fputs("\n", fd);
	fputs(ptr->element->comment, fd);
	fputs("\n", fd);
	ptr = ptr->next;
    }
    
    fclose(fd);
    rename(temp, filename);
    dirty = False;
    return True;
}

// Return a linked list of results.  Find all the last names
// which begin with the character lastPrefix.  Return NULL if there
// are no matches.  The result should be freed with free().  Since our
// linked list is sorted, the result will be also sorted.

DataList *Data::findLastName(char lastPrefix)
{
    DataList *list;
    int numList, sizeList, each;
    DataList *ptr;

    // Create an array of elements for our results.  We will change
    // the next pointers at the end to simulate a linked list.

    numList = 0;
    sizeList = 8;
    list = (DataList *) malloc(sizeList*sizeof(DataList));

    // Traverse our linked list of elements and find the matches.
    // Add the matches to our array, growing the array as necessary.

    ptr = head;
    while (ptr->next)
    {
	ptr = ptr->next;
	if (upper(ptr->element->name[0]) == lastPrefix)
	{
	    if (numList == sizeList)
	    {
		sizeList = 2*sizeList;
		list = (DataList *) realloc(list, sizeList*sizeof(DataList));
	    }
	    list[numList].element = ptr->element;
	    numList++;
	}
    }

    if (!numList)
    {
	free(list);
	return NULL;
    }
    else
    {
	// Now fix up next pointers
	for (each=0; each<numList-1; each++)
	    list[each].next = list+each+1;
	list[numList-1].next = NULL;
	return list;
    }
}

// Add a new entry to the database.

void Data::addEntry(char *name, char *phone, char *addr, char *comment)
{
    DataElement *element;

    dirty = True;
    element = new DataElement();
    element->name = strdup(name);
    element->phone = strdup(phone);
    element->addr = strdup(addr);
    element->comment = strdup(comment);
    insertElement(element);
}

// Remove an entry from the database.  Traverse the linked list and
// unlink the existing element.

void Data::removeEntry(DataElement *element)
{
    DataList *ptr, *list;

    ptr = head;
    while (ptr->next && ptr->next->element != element)
	ptr = ptr->next;

    if (ptr->next && ptr->next->element == element)
    {
	dirty = True;
	list = ptr->next;
	ptr->next = list->next;
	delete list->element;
	delete list;
    }
}

// Modify the fields of an existing entry.  If the name changes,
// we have to reinsert the new element.

void Data::changeEntry(DataElement *element,
		       char *name, char *phone, char *addr, char *comment)
{
    Boolean doSort;

    dirty = True;
    doSort = strcmp(name, element->name);
    free(element->name);
    free(element->phone);
    free(element->addr);
    free(element->comment);
    element->name = strdup(name);
    element->phone = strdup(phone);
    element->addr = strdup(addr);
    element->comment = strdup(comment);

    if (doSort) 
	resort(element);
}

// Return the upper-case version of the character ch.  Used for
// case-insensitive matching.

char Data::upper(char ch)
{
    if (ch >= 'a' && ch <= 'z') 
	return ch-'a'+'A';
    else
	return ch;
}

// Free all of our data.  We do this on exit, and when we load a
// new database.

void Data::deleteData()
{
    DataList *ptr, *del;

    ptr = head->next;

    while (ptr)
    {
	del = ptr;
	ptr = ptr->next;
	delete del->element;
	delete del;
    }

    head->next = NULL;
}

// Insert a new element into our linked-list, preserving sorting.

void Data::insertElement(DataElement *element)
{
    DataList *list, *ptr;

    list = new DataList();
    list->element = element;
    ptr = head;
    while (ptr->next &&
	   strcasecmp(ptr->next->element->name, element->name) < 1) 
	ptr = ptr->next;

    list->next = ptr->next;
    ptr->next = list;
}

// Insert the new element into the proper place in the list, preserving
// sorting.  We do this by finding the elelemt, unlinking it, and
// then inserting it again.

void Data::resort(DataElement *element)
{
    DataList *ptr, *list;

    ptr = head;
    while (ptr->next)
    {
	if (ptr->next->element == element)
	{
	    list = ptr->next;
	    ptr->next = list->next;
	    delete list;
	    insertElement(element);
	    return;
	}
	else
	{
	    ptr = ptr->next;
	}
    }
}


