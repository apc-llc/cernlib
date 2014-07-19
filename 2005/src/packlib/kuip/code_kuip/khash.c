/*
 * $Id: khash.c,v 1.1.1.1 1996/03/08 15:32:56 mclareni Exp $
 *
 * $Log: khash.c,v $
 * Revision 1.1.1.1  1996/03/08 15:32:56  mclareni
 * Kuip
 *
 */
/*CMZ :  2.06/00 23/09/94  15.15.31  by  Alfred Nathaniel*/
/*-- Author :    Alfred Nathaniel   18/11/92*/
/* khash.c: hash table management */

#include "kuip/kuip.h"
#include "kuip/khash.h"

/*
 * compute a hash value from a character string
 *
 * algorithm by P.J. Weinberger
 *
 * reference: page 436 in A.V. Aho, R. Sethi, J.D. Ullman,
 *           "Compilers - Principles, Techniques, and Tools", Addison-Wesley
 */
static
int hashpjw( HashTable* table,
            const char *name )
{
  int fold_case = (table->strcmp != strcmp);
  unsigned int h = 0;
  unsigned int g;

  while( *name != '\0' ) {
    char c = *name++;

    if( fold_case && isupper( c ) )
      c = tolower( c );

    h = (h << 4) + c;
    if( (g = (h & 0xF0000000)) != 0 ) {
      h ^= (g >> 24);
      h ^= g;
    }
  }
  return h % table->size;
}


/*
 * compare two hash entry names (used by qsort)
 */
static
int hash_cmp( const void* p1,
             const void* p2 )
{
  HashArray *ent1 = (HashArray*)p1;
  HashArray *ent2 = (HashArray*)p2;
  return strcmp( ent1->name, ent2->name );
}

static
int hash_casecmp( const void* p1,
                 const void* p2 )
{
  HashArray *ent1 = (HashArray*)p1;
  HashArray *ent2 = (HashArray*)p2;
  return strcasecmp( ent1->name, ent2->name );
}


/*
 * create a new hash table
 */
HashTable *hash_create( int size )
{
  HashTable *table = (HashTable*)malloc( sizeof(HashTable) );
  table->size = size;
  table->entries = (HashEntry**)calloc( size, sizeof(HashEntry*) );
  table->nentries = 0;
  hash_config( table, "string" );
  hash_config( table, "keep" );
  hash_config( table, "ignore" );
  return table;
}


void hash_config( HashTable* table,
                 const char* option )
{
  if( strcmp( option, "string" ) == 0 ) {
    table->is_string = 1;
    table->tag_only = 0;
  }
  else if( strcmp( option, "struct" ) == 0 ) {
    table->is_string = 0;
    table->tag_only = 0;
  }
  else if( strcmp( option, "tag_only" ) == 0 ) {
    table->is_string = 0;
    table->tag_only = 1;
  }
  else if( strcmp( option, "keep" ) == 0 ) {
    table->case_convert = 0;
  }
  else if( strcmp( option, "upper" ) == 0 ) {
    table->case_convert = 1;
  }
  else if( strcmp( option, "lower" ) == 0 ) {
    table->case_convert = -1;
  }
  else if( strcmp( option, "respect" ) == 0 ) {
    table->strcmp = strcmp;
    table->hash_cmp = hash_cmp;
  }
  else if( strcmp( option, "ignore" ) == 0 ) {
    table->strcmp = strcasecmp;
    table->hash_cmp = hash_casecmp;
  }
  else {
    printf( " Invalid option %s for hash_config\n", option );
  }
}


/*
 * delete a hash table freeing all space
 */
void hash_destroy( HashTable *table )
{
  hash_clear( table );
  free( (char*)table->entries );
  free( (char*)table );
}


/*
 * insert the (name,value,tag) data into the hash table
 */
void hash_insert( HashTable *table,
                 const char *name,
                 const void *value,
                 int tag )
{
  int h = hashpjw( table, name );
  HashEntry *ent = table->entries[h];

  if( table->tag_only )
    value = "";
  else if( value == NULL )
    return;

  if( ent == NULL ) {
    /* create a new link head */
    table->nentries++;
    ent = table->entries[h] = (HashEntry*)malloc( sizeof(HashEntry) );
    ent->next = NULL;
    ent->name = strdup( name );
#ifdef DBMALLOC
    malloc_mark( ent );
    malloc_mark( ent->name );
#endif
  }
  else {
    /* search through the list */
    HashEntry *prev = ent;
    while( ent != NULL ) {
      if( (*table->strcmp)( ent->name, name ) == 0 ) {
        /* name exists: replace value */
        strcpy( ent->name, name );
        if( table->is_string )
          free( (char*)ent->value );
        break;
      }
      prev = ent;
      ent = ent->next;
    }
    if( ent == NULL ) {
      /* create a new entry */
      table->nentries++;
      ent = prev->next = (HashEntry*)malloc( sizeof(HashEntry) );
      ent->next = NULL;
      ent->name = strdup( name );
#ifdef DBMALLOC
      malloc_mark( ent );
      malloc_mark( ent->name );
#endif
    }
  }
  if( table->case_convert > 0 )
    strupper( ent->name );
  else if( table->case_convert < 0 )
    strlower( ent->name );

  if( table->is_string ) {
    ent->value = (void*)strdup( (char*)value );
#ifdef DBMALLOC
    malloc_mark( ent->value );
#endif
  }
  else {
    ent->value = (void*)value;
  }
  ent->tag = tag;
}


/*
 * delete all hash entries from table
 */
void hash_clear( HashTable *table )
{
  int i;

  for( i = 0; i < table->size; i++ ) {
    HashEntry *ent = table->entries[i];

    while( ent != NULL ) {
      HashEntry *next = ent->next;

      free( ent->name );
      if( table->is_string )
        free( (char*)ent->value );
      free( (char*)ent );
      ent = next;
    }
    table->entries[i] = NULL;
  }
  table->nentries = 0;
}


/*
 * delete one entry from the hash table
 */
void *hash_remove( HashTable *table,
                  const char *name )
{
  int h = hashpjw( table, name );
  HashEntry *ent;
  HashEntry **prev_next = &table->entries[h];
  void *value = NULL;

  for( ent = table->entries[h]; ent != NULL; ent = ent->next ) {

    if( (*table->strcmp)( ent->name, name ) == 0 ) {
      *prev_next = ent->next;
      free( ent->name );
      if( table->is_string )
        free( (char*)ent->value );
      else if( table->tag_only )
        value = "";
      else
        value = ent->value;
      free( (char*)ent );
      table->nentries--;
      break;
    }
    prev_next = &ent->next;
  }

  return value;
}


/*
 * find an entry and return its value
 */
void *hash_lookup( HashTable *table,
                  const char *name,
                  int *tag_return )
{
  int h = hashpjw( table, name );
  HashEntry *ent = table->entries[h];
  void *value = NULL;

  while( ent != NULL ) {
    if( (*table->strcmp)( ent->name, name ) == 0 ) {
      value = ent->value;
      if( tag_return != NULL )
        *tag_return = ent->tag;
      break;
    }
    ent = ent->next;
  }

  return value;
}


/*
 * return the number of hash entries
 */
int hash_entries( HashTable *table )
{
  return table == NULL ? 0 : table->nentries;
}


/*
 * sort all table entries in a linear array
 */
HashArray *hash_array( HashTable *table )
{
  HashArray *linear = NULL;

  if( table->nentries > 0 ) {
    int n = 0;
    int i;

    linear = (HashArray*)malloc( table->nentries * sizeof(HashArray) );

    for( i = 0; i < table->size; i++ ) {
      HashEntry *ent = table->entries[i];

      while( ent != NULL ) {
        linear[n].name = ent->name;
        linear[n].value = ent->value;
        linear[n].tag = ent->tag;
        n++;
        ent = ent->next;
      }
    }
    qsort( linear, n, sizeof(HashArray), table->hash_cmp );
  }
  return linear;
}
