*
* $Id: dlfcnh.inc,v 1.1.1.1 1996/02/26 17:16:46 mclareni Exp $
*
* $Log: dlfcnh.inc,v $
* Revision 1.1.1.1  1996/02/26 17:16:46  mclareni
* Comis
*
*
*
* dlfcnh.inc
*
/*CMZ :  1.18/02 13/04/94  17.40.02  by  Fons Rademakers*/
/*-- Author :    Vladimir Berezhnoi   01/04/94*/
/*
 * @(#)dlfcn.h	1.3 revision of 92/12/27  20:58:32
 * This is an unpublished work copyright (c) 1992 Helios Software GmbH
 * 3000 Hannover 1, Germany
 */

/*
 * Mode flags for the dlopen routine.
 */
#define RTLD_LAZY	1
#define RTLD_NOW	2

/*
 * To be able to intialize, a library may provide a dl_info structure
 * that contains functions to be called to initialize and terminate.
 */
struct dl_info {
	void (*init)(void);
	void (*fini)(void);
};

#if __STDC__ || defined(_IBMR2)
void *dlopen(const char *path, int mode);
void *dlsym(void *handle, const char *symbol);
char *dlerror(void);
int dlclose(void *handle);
#else
void *dlopen();
void *dlsym();
char *dlerror();
int dlclose();
#endif
