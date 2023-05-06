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
//
// This object handles the printing of the database to a file.  Its
// formatting capabilities are very limited.
//

#include "Print.h"
#include "Preferences.h"

Print::Print()
{
    
}

Print::~Print()
{
    
}

// Print the database to a file

Boolean Print::saveFile(char *filename)
{
    FILE *fd;
    DataList *ptr;
    
    fd = fopen(filename, "w");
    if (!fd) 
	return False;

    ptr = theData->allData();
    while (ptr)
    {
	outputElement(fd, ptr->element);
	ptr = ptr->next;
    }
    fclose(fd);
    return True;
}

// Print one element of the database to the open file.  On the first
// line, print the name and right-justify the phone number.  One
// the next lines, print the address and comment if available.

void Print::outputElement(FILE *fd, DataElement *element)
{
    int num, each;
    
    fputs(element->name, fd);
    num = thePreferences->printWidth()-strlen(element->name)-
	strlen(element->phone);
    if (num > 0)
	for (each=0; each<num; each++)
	    fputs(" ", fd);

    fputs(element->phone, fd);
    fputs("\n", fd);

    if (strlen(element->addr))
	outputString(fd, element->addr, 2, thePreferences->printWidth());
    if (strlen(element->comment))
	outputString(fd, element->comment, 2, thePreferences->printWidth());
}

// Print a string to the open file, with an appropriate indent.  Wrap
// the string as necessary.

void Print::outputString(FILE *fd, char *str, int indent, int width)
{
    int each;
    char *p;
    
    for (each=0; each<indent; each++) 
	fputs(" ", fd);

    if (strlen(str) <= width-indent)
    {
	fputs(str, fd);
	fputs("\n", fd);
    }
    else
    {
	p = str+width-indent;
	while (p > str && *p != ' ')
	    p--;

	if (p > str && *p == ' ')
	{
	    *p = '\0';
	    fputs(str, fd);
	    fputs("\n", fd);
	    *p = ' ';
	    while (*p == ' ')
	    {
		p++;
		outputString(fd, p, indent, width);
	    }
	}
    }
}
