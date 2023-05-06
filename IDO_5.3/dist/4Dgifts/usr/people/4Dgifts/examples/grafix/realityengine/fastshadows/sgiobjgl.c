/*
 *	sgiobjgl -
 *		A simple object format - graphics support.
 *
 *			Paul Haeberli - 1990
 */
#include "stdio.h"
#include "gl.h"
#include "sgiobj.h"
#include "spin.h"

sgiobj *newquadobj(nquads)
int nquads;
{
    sgiobj *obj;

    obj = (sgiobj *)malloc(sizeof(sgiobj));
    obj->next = 0;
    obj->objtype = OBJ_QUADLIST;
    obj->nlongs = PNTLONGS*4*nquads;
    obj->data = (long *)malloc(obj->nlongs*sizeof(long));
    obj->xnlongs = 0;
    obj->xdata = 0;
    return obj;
}

sgiobj *newtriobj(ntri)
int ntri;
{
    sgiobj *obj;

    obj = (sgiobj *)malloc(sizeof(sgiobj));
    obj->next = 0;
    obj->objtype = OBJ_TRILIST;
    obj->nlongs = PNTLONGS*3*ntri;
    obj->data = (long *)malloc(obj->nlongs*sizeof(long));
    obj->xnlongs = 0;
    obj->xdata = 0;
    return obj;
}

sgiobj *newtmeshobj(nlongs)
int nlongs;
{
    sgiobj *obj;

    obj = (sgiobj *)malloc(sizeof(sgiobj));
    obj->next = 0;
    obj->objtype = OBJ_TRIMESH;
    obj->nlongs = nlongs;
    obj->data = (long *)malloc(obj->nlongs*sizeof(long));
    obj->xnlongs = 0;
    obj->xdata = 0;
    return obj;
}

sgiobj *readsgiobj(name)
char *name;
{
    FILE *inf;
    sgiobj *obj, *head, *tail;
    int npoints, colors;
    long objtype, plongs, nlongs;
    long magic;
    int i, ntri, nquads;

    inf = fopen(name,"r");
    if(!inf) {
        fprintf(stderr,"readsgiobj: can't open input file %s\n",name);
        exit(1);
    }
    fread(&magic,sizeof(long),1,inf);
    if(magic == FASTMAGIC) {
	fread(&npoints,sizeof(long),1,inf);
	fread(&colors,sizeof(long),1,inf);
	nquads = npoints/4; 
	if(colors) {
	    fprintf(stderr,"readsgiobj: can't read nonormal spin objects\n");
	    exit(1);
	} else
	    obj = newquadobj(nquads);
	bzero(obj->data,obj->nlongs*sizeof(long));
	for(i=0; i<npoints; i++) {
	    fread(obj->data+(PNTLONGS*i)+OFFSET_NORMAL,3*sizeof(long),1,inf);
	    fread(obj->data+(PNTLONGS*i)+OFFSET_POINT,3*sizeof(long),1,inf);
	    bcopy(obj->data+(PNTLONGS*i)+OFFSET_POINT,
		  obj->data+(PNTLONGS*i)+OFFSET_UVS,3*sizeof(long),1,inf);
	}
	fclose(inf);
	return obj;
    } 
    if(magic == SOMAGIC) {
	head = 0;
	while(1) {
	    fread(&objtype,sizeof(long),1,inf);
	    if(objtype == OBJ_END)
		break;
	    fread(&nlongs,sizeof(long),1,inf);
	    switch(objtype) {
		case OBJ_QUADLIST:
		    nquads = (nlongs/PNTLONGS)/4;
		    obj  = newquadobj(nquads);
		    fread(obj->data,nlongs*sizeof(long),1,inf);
		    break;
		case OBJ_TRILIST:
		    ntri = (nlongs/PNTLONGS)/3;
		    obj  = newtriobj(ntri);
		    fread(obj->data,nlongs*sizeof(long),1,inf);
		    break;
		case OBJ_TRIMESH:
		    obj = newtmeshobj(nlongs);
		    fread(obj->data,nlongs*sizeof(long),1,inf);
		    break;
		default:
		    fprintf(stderr,"readsgiobj: bad obj type %d\n",objtype);
		    exit(1);
		    break;
	    }
	    if(head == 0) {
		head = tail = obj;
	    } else {
		tail->next = obj;
		tail = obj;
	    }
	}
	fclose(inf);
	return head;
    }
    fprintf(stderr,"readsgiobj: bad magic %d in object file\n",magic);
    exit(1);
}


#define BYTEOFF(v)	(sizeof(long)*(v))

drawsgiobj(obj,how)
sgiobj *obj;
int how;
{
    long npolys;
    long *data;
    char *vertdata, *avert;
    int vertlongs, nverts;

    data = obj->data;
    if(obj->objtype == OBJ_QUADLIST) {
	npolys = (obj->nlongs/PNTLONGS)/4;
	switch(how) {
	    case DRAW_POINTS:
		while(npolys--) {
		    bgntmesh();
		    v3f((float*)&data[(PNTLONGS*0)+OFFSET_POINT]);
		    v3f((float*)&data[(PNTLONGS*1)+OFFSET_POINT]);
		    v3f((float*)&data[(PNTLONGS*3)+OFFSET_POINT]);
		    v3f((float*)&data[(PNTLONGS*2)+OFFSET_POINT]);
		    endtmesh();
		    data += PNTLONGS*4;
		}
		break;
	    case DRAW_POINTS|DRAW_NORMALS:
		while(npolys--) {
		    bgntmesh();
		    n3f((float*)&data[(PNTLONGS*0)+OFFSET_NORMAL]);
		    v3f((float*)&data[(PNTLONGS*0)+OFFSET_POINT]);
		    n3f((float*)&data[(PNTLONGS*1)+OFFSET_NORMAL]);
		    v3f((float*)&data[(PNTLONGS*1)+OFFSET_POINT]);
		    n3f((float*)&data[(PNTLONGS*3)+OFFSET_NORMAL]);
		    v3f((float*)&data[(PNTLONGS*3)+OFFSET_POINT]);
		    n3f((float*)&data[(PNTLONGS*2)+OFFSET_NORMAL]);
		    v3f((float*)&data[(PNTLONGS*2)+OFFSET_POINT]);
		    endtmesh();
		    data += PNTLONGS*4;
		}
		break;
	    case DRAW_POINTS|DRAW_UVS:
		while(npolys--) {
		    bgntmesh();
		    t2f((float*)&data[(PNTLONGS*0)+OFFSET_UVS]);
		    v3f((float*)&data[(PNTLONGS*0)+OFFSET_POINT]);
		    t2f((float*)&data[(PNTLONGS*1)+OFFSET_UVS]);
		    v3f((float*)&data[(PNTLONGS*1)+OFFSET_POINT]);
		    t2f((float*)&data[(PNTLONGS*3)+OFFSET_UVS]);
		    v3f((float*)&data[(PNTLONGS*3)+OFFSET_POINT]);
		    t2f((float*)&data[(PNTLONGS*2)+OFFSET_UVS]);
		    v3f((float*)&data[(PNTLONGS*2)+OFFSET_POINT]);
		    endtmesh();
		    data += PNTLONGS*4;
		}
		break;
	    case DRAW_POINTS|DRAW_UVS|DRAW_NORMALS:
		while(npolys--) {
		    bgntmesh();
		    t2f((float*)&data[(PNTLONGS*0)+OFFSET_UVS]);
		    n3f((float*)&data[(PNTLONGS*0)+OFFSET_NORMAL]);
		    v3f((float*)&data[(PNTLONGS*0)+OFFSET_POINT]);
		    t2f((float*)&data[(PNTLONGS*1)+OFFSET_UVS]);
		    n3f((float*)&data[(PNTLONGS*1)+OFFSET_NORMAL]);
		    v3f((float*)&data[(PNTLONGS*1)+OFFSET_POINT]);
		    t2f((float*)&data[(PNTLONGS*3)+OFFSET_UVS]);
		    n3f((float*)&data[(PNTLONGS*3)+OFFSET_NORMAL]);
		    v3f((float*)&data[(PNTLONGS*3)+OFFSET_POINT]);
		    t2f((float*)&data[(PNTLONGS*2)+OFFSET_UVS]);
		    n3f((float*)&data[(PNTLONGS*2)+OFFSET_NORMAL]);
		    v3f((float*)&data[(PNTLONGS*2)+OFFSET_POINT]);
		    endtmesh();
		    data += PNTLONGS*4;
		}
		break;
	    case DRAW_POINTS|DRAW_COLORS:
		while(npolys--) {
		    bgntmesh();
		    c3f((float*)&data[(PNTLONGS*0)+OFFSET_COLOR]);
		    v3f((float*)&data[(PNTLONGS*0)+OFFSET_POINT]);
		    c3f((float*)&data[(PNTLONGS*1)+OFFSET_COLOR]);
		    v3f((float*)&data[(PNTLONGS*1)+OFFSET_POINT]);
		    c3f((float*)&data[(PNTLONGS*3)+OFFSET_COLOR]);
		    v3f((float*)&data[(PNTLONGS*3)+OFFSET_POINT]);
		    c3f((float*)&data[(PNTLONGS*2)+OFFSET_COLOR]);
		    v3f((float*)&data[(PNTLONGS*2)+OFFSET_POINT]);
		    endtmesh();
		    data += PNTLONGS*4;
		}
		break;
	    case DRAW_POINTS|DRAW_COLORS|DRAW_NORMALS:
		while(npolys--) {
		    bgntmesh();
		    c3f((float*)&data[(PNTLONGS*0)+OFFSET_COLOR]);
		    n3f((float*)&data[(PNTLONGS*0)+OFFSET_NORMAL]);
		    v3f((float*)&data[(PNTLONGS*0)+OFFSET_POINT]);
		    c3f((float*)&data[(PNTLONGS*1)+OFFSET_COLOR]);
		    n3f((float*)&data[(PNTLONGS*1)+OFFSET_NORMAL]);
		    v3f((float*)&data[(PNTLONGS*1)+OFFSET_POINT]);
		    c3f((float*)&data[(PNTLONGS*3)+OFFSET_COLOR]);
		    n3f((float*)&data[(PNTLONGS*3)+OFFSET_NORMAL]);
		    v3f((float*)&data[(PNTLONGS*3)+OFFSET_POINT]);
		    c3f((float*)&data[(PNTLONGS*2)+OFFSET_COLOR]);
		    n3f((float*)&data[(PNTLONGS*2)+OFFSET_NORMAL]);
		    v3f((float*)&data[(PNTLONGS*2)+OFFSET_POINT]);
		    endtmesh();
		    data += PNTLONGS*4;
		}
		break;
	}
    } else if(obj->objtype == OBJ_TRILIST) {
	npolys = (obj->nlongs/PNTLONGS)/3;
	switch(how) {
	    case DRAW_POINTS:
		while(npolys--) {
		    bgntmesh();
		    v3f((float*)&data[(PNTLONGS*0)+OFFSET_POINT]);
		    v3f((float*)&data[(PNTLONGS*1)+OFFSET_POINT]);
		    v3f((float*)&data[(PNTLONGS*2)+OFFSET_POINT]);
		    endtmesh();
		    data += PNTLONGS*3;
		}
		break;
	    case DRAW_POINTS|DRAW_NORMALS:
		while(npolys--) {
		    bgntmesh();
		    n3f((float*)&data[(PNTLONGS*0)+OFFSET_NORMAL]);
		    v3f((float*)&data[(PNTLONGS*0)+OFFSET_POINT]);
		    n3f((float*)&data[(PNTLONGS*1)+OFFSET_NORMAL]);
		    v3f((float*)&data[(PNTLONGS*1)+OFFSET_POINT]);
		    n3f((float*)&data[(PNTLONGS*2)+OFFSET_NORMAL]);
		    v3f((float*)&data[(PNTLONGS*2)+OFFSET_POINT]);
		    endtmesh();
		    data += PNTLONGS*3;
		}
		break;
	    case DRAW_POINTS|DRAW_UVS:
		while(npolys--) {
		    bgntmesh();
		    t2f((float*)&data[(PNTLONGS*0)+OFFSET_UVS]);
		    v3f((float*)&data[(PNTLONGS*0)+OFFSET_POINT]);
		    t2f((float*)(float*)&data[(PNTLONGS*1)+OFFSET_UVS]);
		    v3f((float*)&data[(PNTLONGS*1)+OFFSET_POINT]);
		    t2f((float*)&data[(PNTLONGS*2)+OFFSET_UVS]);
		    v3f((float*)&data[(PNTLONGS*2)+OFFSET_POINT]);
		    endtmesh();
		    data += PNTLONGS*3;
		}
		break;
	    case DRAW_POINTS|DRAW_UVS|DRAW_NORMALS:
		while(npolys--) {
		    bgntmesh();
		    n3f((float*)&data[(PNTLONGS*0)+OFFSET_NORMAL]);
		    t2f((float*)&data[(PNTLONGS*0)+OFFSET_UVS]);
		    v3f((float*)&data[(PNTLONGS*0)+OFFSET_POINT]);
		    n3f((float*)&data[(PNTLONGS*1)+OFFSET_NORMAL]);
		    t2f((float*)&data[(PNTLONGS*1)+OFFSET_UVS]);
		    v3f((float*)&data[(PNTLONGS*1)+OFFSET_POINT]);
		    n3f((float*)&data[(PNTLONGS*2)+OFFSET_NORMAL]);
		    t2f((float*)&data[(PNTLONGS*2)+OFFSET_UVS]);
		    v3f((float*)&data[(PNTLONGS*2)+OFFSET_POINT]);
		    endtmesh();
		    data += PNTLONGS*3;
		}
		break;
	    case DRAW_POINTS|DRAW_COLORS:
		while(npolys--) {
		    bgntmesh();
		    c3f((float*)&data[(PNTLONGS*0)+OFFSET_COLOR]);
		    v3f((float*)&data[(PNTLONGS*0)+OFFSET_POINT]);
		    c3f((float*)&data[(PNTLONGS*1)+OFFSET_COLOR]);
		    v3f((float*)&data[(PNTLONGS*1)+OFFSET_POINT]);
		    c3f((float*)&data[(PNTLONGS*2)+OFFSET_COLOR]);
		    v3f((float*)&data[(PNTLONGS*2)+OFFSET_POINT]);
		    endtmesh();
		    data += PNTLONGS*3;
		}
		break;
	    case DRAW_POINTS|DRAW_COLORS|DRAW_NORMALS:
		while(npolys--) {
		    bgntmesh();
		    c3f((float*)&data[(PNTLONGS*0)+OFFSET_COLOR]);
		    n3f((float*)&data[(PNTLONGS*0)+OFFSET_NORMAL]);
		    v3f((float*)&data[(PNTLONGS*0)+OFFSET_POINT]);
		    c3f((float*)&data[(PNTLONGS*1)+OFFSET_COLOR]);
		    n3f((float*)&data[(PNTLONGS*1)+OFFSET_NORMAL]);
		    v3f((float*)&data[(PNTLONGS*1)+OFFSET_POINT]);
		    c3f((float*)&data[(PNTLONGS*2)+OFFSET_COLOR]);
		    n3f((float*)&data[(PNTLONGS*2)+OFFSET_NORMAL]);
		    v3f((float*)&data[(PNTLONGS*2)+OFFSET_POINT]);
		    endtmesh();
		    data += PNTLONGS*3;
		}
		break;
	}
    } else if(obj->objtype == OBJ_TRIMESH) {
	if(how == DRAW_LINES)
	    return;
	else {
	    vertlongs = *data++;
	    vertdata = (char *)data;
	    data += vertlongs;
	    while(1) {
		switch(*data++) {
		    case OP_BGNTMESH:
			bgntmesh();
			break;
		    case OP_SWAPTMESH:
			swaptmesh();
			break;
		    case OP_ENDBGNTMESH:
			endtmesh();
			bgntmesh();
			break;
		    case OP_ENDTMESH:
			endtmesh();
			return;
		    default:
			fprintf(stderr,"drawsgiobj: bad tmesh op %d\n",*data);
			exit(1);
		}
		nverts = *data++;
		switch(how) {
		    case DRAW_POINTS:
			while(nverts--) {
			    avert = vertdata + *data++;
			    v3f((float*)(avert+BYTEOFF(OFFSET_POINT)));
			}
			break;
		    case DRAW_POINTS|DRAW_NORMALS:
			while(nverts--) {
			    avert = vertdata + *data++;
			    n3f((float*)(avert+BYTEOFF(OFFSET_NORMAL)));
			    v3f((float*)(avert+BYTEOFF(OFFSET_POINT)));
			}
			break;
		    case DRAW_POINTS|DRAW_UVS:
			while(nverts--) {
			    avert = vertdata + *data++;
			    t2f((float*)(avert+BYTEOFF(OFFSET_UVS)));
			    v3f((float*)(avert+BYTEOFF(OFFSET_POINT)));
			}
			break;
		    case DRAW_POINTS|DRAW_UVS|DRAW_NORMALS:
			while(nverts--) {
			    avert = vertdata + *data++;
			    t2f((float*)(avert+BYTEOFF(OFFSET_UVS)));
			    n3f((float*)(avert+BYTEOFF(OFFSET_NORMAL)));
			    v3f((float*)(avert+BYTEOFF(OFFSET_POINT)));
			}
			break;
		    case DRAW_POINTS|DRAW_COLORS:
			while(nverts--) {
			    avert = vertdata + *data++;
			    c3f((float*)(avert+BYTEOFF(OFFSET_COLOR)));
			    v3f((float*)(avert+BYTEOFF(OFFSET_POINT)));
			}
			break;
		    case DRAW_POINTS|DRAW_COLORS|DRAW_NORMALS:
			while(nverts--) {
			    avert = vertdata + *data++;
			    c3f((float*)(avert+BYTEOFF(OFFSET_COLOR)));
			    n3f((float*)(avert+BYTEOFF(OFFSET_NORMAL)));
			    v3f((float*)(avert+BYTEOFF(OFFSET_POINT)));
			}
		}
	    }
	}
    } else {
	fprintf(stderr,"drawsgiobj: bad object type %d\n",obj->objtype);
	exit(1);
    }
}
