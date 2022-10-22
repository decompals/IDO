/*
 * Copyright (C) 1992 Dirk Grunwald and Benjamin Zorn
 * Mostly written by Dirk Grunwald (grunwald@cs.colorado.edu)
 * 
 * This file is part of CustoMalloc.
 * 
 * CustoMalloc is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation (version 2).
 * 
 * CustoMalloc is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with CustoMalloc; see the file COPYING.  If not, write to
 * the authors
 */

#ifndef _Database_h_
#  include "Database.h"
#endif

#include <stdio.h>
#include <assert.h>
#include <sys/types.h>
#include <unistd.h>
#include <sys/stat.h>
#include <fcntl.h>

static int __cache_file_no = -1;
static char *__cache_file_name = 0;

#ifndef DataBaseFileName
extern char DataBaseFileName[BUFSIZ]; /* from Generate.c */
#endif

CacheItem *customalloc_read_database()
{
  extern char *getenv(char*);
  __cache_file_name = getenv("CustoMalloc");
  if ( !__cache_file_name ) {
    __cache_file_name = DataBaseFileName;
  }
  __cache_file_no = open(__cache_file_name, O_RDONLY, 0);

  if ( __cache_file_no >= 0 ) {
    CacheItem *root ;
    CacheItem *Root = 0;

    int size = lseek(__cache_file_no, 0L, SEEK_END);
    
    if ( size % sizeof(CacheItem) != 0 ) {
      fprintf (stderr, "Size = %d, sizeof(CacheItem) = %d\n",
	       size, sizeof(CacheItem) );
      abort();
    }
    
    lseek(__cache_file_no, 0L, SEEK_SET);
    

#ifdef _INTERNAL_MALLOC_
    root = (CacheItem *) __internal_malloc(size);
#else
    root = (CacheItem *) malloc(size);
#endif

    if ( root != 0 ) {
      int count = read(__cache_file_no, root, size);
      close(__cache_file_no);
      
      assert( count == size );

      Root = root;
      while (root != 0 ) {
	CacheItem *old_next = root -> next;
	root -> next = (root + 1);
	root -> root = 0;

	root -> objects_in_free_list = 0;
	root -> last_event = 0;

	if ( old_next == 0 ) break;
	root++;
      }
      root -> next = 0;
    }
    return ( Root );
  } else {
    return( 0 );
  }
}

void customalloc_write_database(CacheItem *Root)
{
  __cache_file_no = open(__cache_file_name,
			 O_WRONLY | O_APPEND | O_CREAT | O_TRUNC,
			 0644 );
  if ( __cache_file_no >= 0 ) {
    CacheItem *root;
    for ( root = Root; root != 0; root = root -> next ) {
      int bytes;
      root -> runs += 1;
      bytes = write(__cache_file_no, root, sizeof(CacheItem) );
      assert( bytes == sizeof(CacheItem) );
    }
    close(__cache_file_no);
  } else {
    perror("open@exit");
  }
}
