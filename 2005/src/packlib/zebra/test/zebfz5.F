*
* $Id: zebfz5.F,v 1.1.1.1 1996/03/06 10:46:59 mclareni Exp $
*
* $Log: zebfz5.F,v $
* Revision 1.1.1.1  1996/03/06 10:46:59  mclareni
* Zebra
*
*
#define CERNLIB_QFIMDAX
#define CERNLIB_QFIMDANY
#define CERNLIB_MAXMAK24
#include "tefqmain/tfmain.F"
#include "fqtest/fqtest.F"
#undef CERNLIB_ZEBRA_ZSTATE_INC
#undef CERNLIB_ZEBRA_ZUNIT_INC
#undef CERNLIB_ZEBRA_MZBITS_INC
#undef CERNLIB_ZEBRA_QUEST_INC
#undef CERNLIB_ZEBRA_CQBKC_INC
#include "fqtest/fqmake.F"
#undef CERNLIB_ZEBRA_ZSTATE_INC
#undef CERNLIB_ZEBRA_ZUNIT_INC
#undef CERNLIB_ZEBRA_MZBITS_INC
#undef CERNLIB_ZEBRA_QUEST_INC
#undef CERNLIB_ZEBRA_CQBKC_INC
#include "fqtest/fqback.F"
#include "fqtend/zend.F"
#include "tzebend/qnext.F"
#include "tdzdummy/dzdummy.F"
