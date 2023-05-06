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
// This object holds the current preference values.  Since there will be
// only one of these, the global thePreferences points to this object.
//

#include <stdio.h>
#include <string.h>
#include <malloc.h>
#include "Preferences.h"
#include "path.h"
#include <Vk/VkResource.h>

Preferences *thePreferences = NULL;

Preferences::Preferences(Widget w)
{
    char *name;
    
    dirty = False;
    
    // Initialize the defaults based on resource settings.  This way,
    // preference defaults can be set in our application's app-defaults
    // file.
    
    if (name = VkGetResource(w, "defaultFilename", "DefaultFilename"))
	_filename = pathexpandtilde(name);
    else
	_filename = pathexpandtilde(DEFAULT_FILENAME);

    _showAddr = (Boolean) (int) VkGetResource(w, "showAddr", "ShowAddr",
					      XmRBoolean, "True");
    _showComment = (Boolean) (int) VkGetResource(w, "showComment", "ShowComment",
						 XmRBoolean, "True");
    _itemSpacing = (int) VkGetResource(w, "itemSpacing", "ItemSpacing",
				       XmRInt, "5");
    _printWidth = (int) VkGetResource(w, "printWidth", "PrintWidth",
				      XmRInt, "70");
    
    loadPreferences();
    
    thePreferences = this;
}

Preferences::~Preferences()
{
    free(_filename);
}

// Change where we save our database file

void Preferences::setFilename(const char *v)
{
    free(_filename);
    _filename = pathexpandtilde(v);
}

// Load previously-saved preference settings from disk.  Preferences
// are saved in a different file from the database, one with the
// name concatenated with "Pref".

void Preferences::loadPreferences()
{
    char path[1024], str[1024];
    FILE *fd;
    int num;
    
    sprintf(path, "%sPref", _filename);
    if (fd = fopen(path, "r"))
    {
	while (fgets(str, sizeof(str), fd))
	{
	    if (!strcmp(str, "filename\n"))
	    {
		if (readStr(fd, str))
		    setFilename(str);
		else
		    fprintf(stderr, "PhoneBook: garbled 'filename' preference\n");
	    }
	    else if (!strcmp(str, "showAddr\n"))
	    {
		if (readInt(fd, &num)) 
		    _showAddr = num;
		else
		    fprintf(stderr, "VCal: garbled 'showAddr' preference\n");
	    }
	    else if (!strcmp(str, "showComment\n"))
	    {
		if (readInt(fd, &num))
		    _showComment = num;
		else
		    fprintf(stderr, "VCal: garbled 'showComment' preference\n");
	    }
	    else if (!strcmp(str, "itemSpacing\n"))
	    {
		if (readInt(fd, &num))
		    _itemSpacing = num;
		else
		    fprintf(stderr, "VCal: garbled 'itemSpacing' preference\n");
	    }
	    else if (!strcmp(str, "printWidth\n"))
	    {
		if (readInt(fd, &num))
		    _printWidth = num;
		else
		    fprintf(stderr, "VCal: garbled 'printWidth' preference\n");
	    }
	    else
	    {
		fprintf(stderr, "PhoneBook: unknown preference: %s", str);
		readStr(fd, str);
	    }
	}
	fclose(fd);
	dirty = False;
    }
}

// Save preferences to disk, using the database filename concatenated
// with "Pref".

void Preferences::savePreferences()
{
    char path[1024];
    FILE *fd;
    
    sprintf(path, "%sPref", _filename);
    if (fd = fopen(path, "w"))
    {
	fprintf(fd, "filename\n%s\n", _filename);
	fprintf(fd, "showAddr\n%d\n", _showAddr);
	fprintf(fd, "showComment\n%d\n", _showComment);
	fprintf(fd, "itemSpacing\n%d\n", _itemSpacing);
	fprintf(fd, "printWidth\n%d\n", _printWidth);
	fclose(fd);
	dirty = False;
    }
}

/**********************************************************************/

// Read a string from the open file

int Preferences::readStr(FILE *fd, char *str_return)
{
    static char str[1024];
    int len, str_ct, each;
    
    if (!fgets(str, sizeof(str), fd))
    {
	return 0;
    }
    else
    {
	len = strlen(str);
	if (str[len-1] == '\n')
	{
	    str[len-1] = '\0';
	    len--;
	}
	str_ct = 0;
	for (each=0; each<len; each++)
	{
	    if (str[each] == '\\' && each < len-1 && str[each+1] == 'n') {
		str_return[str_ct] = '\n';
		each++;
	    }
	    else
	    {
		str_return[str_ct] = str[each];
	    }
	    str_ct++;
	}
	str_return[str_ct] = '\0';
	return 1;
    }
}

// Read an integer from the open file

int Preferences::readInt(FILE *fd, int *num)
{
    static char str[1024];
    
    return (fgets(str, sizeof(str), fd) && (sscanf(str, "%d", num) == 1));
}
