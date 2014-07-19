/*
 * $Id: fplocal.h,v 1.1.1.1 1996/03/01 11:39:11 mclareni Exp $
 *
 * $Log: fplocal.h,v $
 * Revision 1.1.1.1  1996/03/01 11:39:11  mclareni
 * Paw
 *
 */
/*CMZ :  2.07/03 21/06/95  15.52.04  by  Yana Gaponenko*/
/*-- Author :    Gregory Kozlovsky   08/04/95*/

/*******************************************************************************
 Locally callable routines of Histogram Fit and Vector Fit Panels.
 =====================> Author: G. Kozlovsky, 1994 <===========================
 ******************************************************************************/
#ifdef __cplusplus
extern "C" {
#endif

/***********************************************************************
        Include files
 **********************************************************************/
#include "fpanels.h"

/***********************************************************************
        Named constants
 **********************************************************************/

#define FPVERSION " (v2.0)"

/***** bits of panelState *****/
#define PanelStateValid 0
#define FitObjInvalid   (1<<0)
#define FitFuncInvalid  (1<<1)
#define FitModelInvalid (1<<2)

/***********************************************************************
        Enumerators
 **********************************************************************/

/****** graphics update mode ******/
enum UpdateModeCode {UPDATE_IMMEDIATE, UPDATE_DELAYED, UPDATE_DISABLED};

/****** the result of the previous reading of the text field before change
 Syntax error means that the field can not be interpreted as a number
 or name and not empty,
 semantic error means that the given value is not suitable for
 the given parameter (negative, or empty, for example)
******/
enum TextPrevStat {
    TEXTSTAT_READOK=0,       /* the previous reading was successful */
    TEXTSTAT_EMPTY=1,        /* the field is empty */
    TEXTSTAT_SYNTERROR=(1<<1),    /* the previos reading gave syntax error */
    TEXTSTAT_SEMANTERROR=(1<<2),   /* semantic error condition is set up */
    TEXTSTAT_SYNTAX=TEXTSTAT_EMPTY|TEXTSTAT_SYNTERROR,
    TEXTSTAT_ANYERROR=TEXTSTAT_SYNTERROR|TEXTSTAT_SEMANTERROR
};

/***** indicate if the text has been changed *****/
enum TextChange {TEXTSTAT_NOCHANGE, TEXTSTAT_CHANGED};

/***** text field status information *****/
typedef struct {
    int change;
    int stat;
} TextStatusInfo;

/****** status of the limits ****
 limits are absent if they are both specified as zero or both fields are
 empty; limits are invalid if lower limit >= than upper limit, or one of
 the fields is empty
******/
typedef enum _LimitsStatCode {
    LIMITSTAT_OK, LIMITSTAT_ABSENT,
    LIMITSTAT_INVALID, LIMITSTAT_PARAMOUT
} LimitsStatCode;

typedef enum _ScaleStatCode {
    SCALESTAT_OK, SCALESTAT_INVALID, SCALESTAT_PARAMOUT
} ScaleStatCode;

/****** fixed/released status of parameter ******/
typedef enum _FixRelStat {PARSTAT_FIXED, PARSTAT_RELED} FixRelStat;

/****** the source and reason for changing the value of a parameter ******/
typedef enum _ParChangeReason {PARCHN_TYPED, PARCHN_OUTSIDE, PARCHN_SCALEDRAG,
    PARCHN_SCALESET, PARCHN_SCALEMINMAX, PARCHN_LIMITS, PARCHN_MINUIT
} ParChangeReason;

/****** the source and reason for changing the value of scale min or max */
typedef enum _MinrMaxChangeReason {MINMAXCHN_TYPED, MINMAXCHN_DOWN,
MINMAXCHN_UP, MINMAXCHN_PARAMTYPED, MINMAXCHN_LIMITS} MinrMaxChangeReason;

/****** return from km_confirm ******/
typedef enum _ConfirmRetCode {CONFIRM_YES=1, CONFIRM_NO=2} ConfirmRetCode;

/****** plot type for hffplot *****/
typedef enum _PlotType {ContPlot, FuncPlot} PlotType;

/***************************************************************************
 Preprocessor macros
***************************************************************************/
#define max(a,b)        (((a) > (b)) ? (a) : (b))
#define min(a,b)        (((a) < (b)) ? (a) : (b))

/***************************************************************************
 External variables
***************************************************************************/

/* extern int mnInitialized; Minuit initialized once in a run */

/***** help text strings called from subpanels *****/
extern char fp_filesel_help_text[];
extern char fp_contpanel_help_text[];
extern char fp_scanpanel_help_text[];
extern char fp_transpanel_help_text[];

/***** cursors *****/
extern Cursor normCursor;
extern Cursor helpCursor;
extern Cursor busyCursor;
extern Cursor blckCursor;
extern Cursor selCursor;

/***************************************************************************
 Default values
***************************************************************************/
typedef struct _SpecResData {
    Dimension textMarginHeight; /* text widget margins */
    Dimension textMarginWidth;
        /***** parameter line default resources *****/
    Dimension parScaleWidth;
    int parScaleResolution; /* scale values are from 0 to XmNmaximum */
    short parNameColumns;   /* text widget number of columns */
    short parValueColumns;
    short parStepColumns;
    short parLimitsColumns;
    short parErrorsColumns;
    short parScaleMinMaxColumns;
    char *parActiveColor;       /* parActiveToggle parameters */
    int parActiveMarginFract;   /* fraction denominator for margins */
    Dimension parBotSeparatorHeight;
        /***** the main panel default resources */
    Dimension topLevelWidth; /* panel size */
    Dimension topLevelHeight;
    char *histoFitTitle;     /* panel title */
    char *vectorFitTitle;    /* panel title */
    Dimension funcAreaOffset; /* offsets between items in function area */
    Dimension statAreaOffset; /* offsets between items in status area */
    Dimension settingsAreaOffset; /* offsets between items in settings area */
    Dimension parScrolledShadow; /* scrolled window shadow thickness */
    Dimension parScrolledSpacing; /* scrolled window spacing */
    Dimension parScrolledCorrection; /* correction for spacing */
    short vectnameColumns;
    short histidColumns;
    short rangeColumns;
    short optnColumns;
    short ffuncColumns;
    short fmodelColumns;
    short fvalColumns;   /* fmin has the same number of columns */
    short fedmColumns;
    short errordefColumns;
    short toleranceColumns;
    short devsColumns;
         /***** contour panel *****/
    short kuipNameColumns;
} SpecResData;

extern SpecResData res_data;

void set_hfresources(void); /* Set resource values */

/***************************************************************************
 Function prototypes
***************************************************************************/

/****** viewmenu.c ******/
void setLineView(void *plct);

void callbackViewLimits(
Widget widget,
XtPointer client_data,
XtPointer call_data);

void callbackViewSteps(
Widget widget,
XtPointer client_data,
XtPointer call_data);

void callbackViewEparab(
Widget widget,
XtPointer client_data,
XtPointer call_data);

void callbackViewEminos(
Widget widget,
XtPointer client_data,
XtPointer call_data);

void callbackViewGlobcc(
Widget widget,
XtPointer client_data,
XtPointer call_data);

void queryPreferredSizes(void *mctxt);
void sizeParLines(void *mctxt, int istart, int nlines);
void resizeParForm(void *mctxt);

void action_resizeParForm(
Widget widget,
XEvent *event,
String *parms,
Cardinal *numParms);

/****** parline.c ******/
int setScaleWorld(Widget wscale, double dval, void *plctxt);
void setParValue(double dval, void *plctxt, ParChangeReason reason);
int setParValueAndScale(double dval, void *plctxt, ParChangeReason reason);

void parChangeDisable(void *plctxt);
void parChangeEnable(void *plctxt);

void createParLines(void *mctxt, int istart, int nlines);
void destroyParLines(void *mctxt, int istart, int nlines);

/****** text.c ******/
void utl_initMarkText(Widget text);
void utl_markTextInvalid(Widget text);
void utl_markTextValid(Widget text);

void textReadFloat(Widget text, TextStatusInfo *status,
    double *dp, int (*valueok)(double dval));
int anyFloatOrEmptyOK(double dval);
int anyFloatEmptyIs0OK(double dval);
int nonEmptyFloatOK(double dval);
int positiveFloatOK(double dval);

void textReadInt(Widget text, TextStatusInfo *status,
    int *ip, int (*valueok)(int dval));
int anyIntOrEmptyOK(int ival);
int anyIntEmptyIs0OK(int ival);
int nonEmptyIntOK(int ival);
int positiveIntOK(int ival);

void textReadName(Widget text, TextStatusInfo *status,
    char *name, int (*valueok)(char *name,int n));
int anyNameOrEmptyOK(char *name, int n);
int nonEmptyNameOK(char *name, int n);

void textReadString(Widget text, TextStatusInfo *status, char *str);
void textWriteFloat(Widget text, TextStatusInfo *status, double dval);
void textWriteInt(Widget text, TextStatusInfo *status, int ival);
void textWriteEmpty(Widget text, TextStatusInfo *status);
void labelWriteFloat(Widget label, double dval);
void textWriteName(Widget text, TextStatusInfo *status, char *tstr);

void textSetSemantError(Widget text, TextStatusInfo *status);
void textRemoveSemantError(Widget text, TextStatusInfo *status);

void clb_textChanged(
Widget widget,
XtPointer client_data,
XtPointer call_data);

/****** mainclbs.c ******/

void clb_setStrategy0(
Widget widget,
XtPointer client_data,
XtPointer call_data);

void clb_setStrategy1(
Widget widget,
XtPointer client_data,
XtPointer call_data);

void clb_setStrategy2(
Widget widget,
XtPointer client_data,
XtPointer call_data);

void clb_setPrintout_1(
Widget widget,
XtPointer client_data,
XtPointer call_data);

void clb_setPrintout0(
Widget widget,
XtPointer client_data,
XtPointer call_data);

void clb_setPrintout1(
Widget widget,
XtPointer client_data,
XtPointer call_data);

void clb_setPrintout2(
Widget widget,
XtPointer client_data,
XtPointer call_data);

void clb_setPrintout3(
Widget widget,
XtPointer client_data,
XtPointer call_data);

void clb_updateImmediate(
Widget widget,
XtPointer client_data,
XtPointer call_data);

void clb_updateDelayed(
Widget widget,
XtPointer client_data,
XtPointer call_data);

void clb_updateDisabled(
Widget widget,
XtPointer client_data,
XtPointer call_data);

void histoRangeWrite(Widget text);

void *createMainPanel();

/****** adjust.c ******/

double roundUpScaleMin(double dmin, double dmax);
double roundDnScaleMin(double dmin, double dmax);
double roundUpScaleMax(double dmin, double dmax);
double roundDnScaleMax(double dmin, double dmax);

void setScaleInterval(double dmin, double dmax, void *plctxt, int reason);

void adjustSetClbs(void *plctxt);

/****** minimenu.c ******/
void clb_minMinimize(
Widget widget,
XtPointer client_data,
XtPointer call_data);

void clb_minMigrad(
Widget widget,
XtPointer client_data,
XtPointer call_data);

void clb_minSeek(
Widget widget,
XtPointer client_data,
XtPointer call_data);

void clb_minSimplex(
Widget widget,
XtPointer client_data,
XtPointer call_data);

void clb_minImprove(
Widget widget,
XtPointer client_data,
XtPointer call_data);

/****** examenu.c ******/
Widget createContourPanel(void *mctxt);
void closeContourPanel(void *mctxt);

void clb_exaContour(
Widget widget,
XtPointer client_data,
XtPointer call_data);

void clb_exaHesse(
Widget widget,
XtPointer client_data,
XtPointer call_data);

void clb_exaMinos(
Widget widget,
XtPointer client_data,
XtPointer call_data);

void clb_exaCorrelation(
Widget widget,
XtPointer client_data,
XtPointer call_data);

void clb_exaCovariance(
Widget widget,
XtPointer client_data,
XtPointer call_data);

void clb_exaEigenvalues(
Widget widget,
XtPointer client_data,
XtPointer call_data);

/****** scanmenu.c ******/
Widget createScanPanel(void *mctxt);
void closeScanPanel(void *mctxt);

void clb_scanOnedim(
Widget widget,
XtPointer client_data,
XtPointer call_data);

/****** filemenu.c ******/
void filemenuSetClbs(void *mctxt);

/****** transmenu.c ******/
Widget createVectPanel(void *mctxt);
void closeVectPanel(void *mctxt);

void transmenuSetClbs(void *mctxt);

/****** helpmenu.c ******/
void helpmenuSetClbs(void *mctxt);

/****** cfort.c ******/
char *strftoc(char *s, int l);
char *strctof(char *s, int l);

void chgive(int id,
    int *nxp, float *xmip, float *xmap,int *nyp, float *ymip, float *ymap);

void hffplot(int npta, double xpta[], double ypta[],
    char xtit[], char ytit[],
    PlotType pt, int ovrl);
int fpsavevc(char vname[], int n, double vect[], Widget panelShell);
void hfgrupdate(void *mctxt);
void vfgrupdate(void *mctxt);

void get_mnerrdef(double *errdefp);
void get_mnname(int iparnum, char parnamei[]);
void get_mnnames(void *ctx);
void get_mnparms(void *ctx, Boolean ifeparab);
void get_mnglobcc(void *ctx);
void get_mneminos(void *ctx);
void set_mnparm(int iparnum, char parnamei[], double vpari,double vstepi,
double vlowi,double vuppi);
void set_mnparms(void *ctx);
Boolean fp_vexist(Widget shell,char *vname);
Boolean fp_hgetfmod(char *fmodname);
Boolean fp_vgetfmod(char *fmodname);
void fp_fitstatupdt();
void fp_hsetdata(int histid,void *mctxt);
void fp_hsetoptn(void *mctxt);
void fp_vsetoptn(void *mctxt);

/****** mutil.c *****/
void utlQueryDimensions(
Widget w,
Dimension *width,
Dimension *height);

/****** busy.c ******/
void createBusyDialog(Widget parent, void *ctxt);
void destroyBusyDialog(Widget parent, void *ctxt);
void processXEvents();


/****** Fortran typedefs *****/
typedef  int      INTEGER;
typedef  int      LOGICAL;
typedef  float    REAL;
typedef  double   DBLPREC;
typedef  INTEGER        INT_FUNCTION();
typedef  INT_FUNCTION  *INT_FUNCPTR;

typedef  void         (*SUBRPTR)();

/****** Fortran function types *****/
typedef void (*FCNTYPE)(int *,double[],double *,double[],int *, void *);
typedef float (*UFCNTYPE)(float *);

#ifdef vms

#  define F77_PROTO_(name) \
     name(void)
#  define F77_CALLS_(name) \
     name()

#  define F77_PROTO_C(name) \
     name(struct dsc$descriptor_s*)
#  define F77_CALLS_C(name,c1,l1) \
     do { \
       struct dsc$descriptor_s _s1; \
       _s1.dsc$b_dtype   = DSC$K_DTYPE_T; \
       _s1.dsc$b_class   = DSC$K_CLASS_S; \
       _s1.dsc$a_pointer = c1; \
       _s1.dsc$w_length  = l1; \
        name(&_s1); \
     } while(0)

#  define F77_PROTO_CC(name) \
     name(struct dsc$descriptor_s*, struct dsc$descriptor_s*)
#  define F77_CALLS_CC(name,c1,l1,c2,l2) \
     do { \
       struct dsc$descriptor_s _s1, _s2; \
       _s1.dsc$b_dtype   = DSC$K_DTYPE_T; \
       _s1.dsc$b_class   = DSC$K_CLASS_S; \
       _s1.dsc$a_pointer = c1; \
       _s1.dsc$w_length  = l1; \
       _s2.dsc$b_dtype   = DSC$K_DTYPE_T; \
       _s2.dsc$b_class   = DSC$K_CLASS_S; \
       _s2.dsc$a_pointer = c2; \
       _s2.dsc$w_length  = l2; \
       name(&_s1, &_s2); \
     } while(0)

#  define F77_PROTO_CCC(name) \
     name(struct dsc$descriptor_s*, struct dsc$descriptor_s*, \
          struct dsc$descriptor_s*)
#  define F77_CALLS_CCC(name,c1,l1,c2,l2,c3,l3) \
     do { \
       struct dsc$descriptor_s _s1, _s2, _s3; \
       _s1.dsc$b_dtype   = DSC$K_DTYPE_T; \
       _s1.dsc$b_class   = DSC$K_CLASS_S; \
       _s1.dsc$a_pointer = c1; \
       _s1.dsc$w_length  = l1; \
       _s2.dsc$b_dtype   = DSC$K_DTYPE_T; \
       _s2.dsc$b_class   = DSC$K_CLASS_S; \
       _s2.dsc$a_pointer = c2; \
       _s2.dsc$w_length  = l2; \
       _s3.dsc$b_dtype   = DSC$K_DTYPE_T; \
       _s3.dsc$b_class   = DSC$K_CLASS_S; \
       _s3.dsc$a_pointer = c3; \
       _s3.dsc$w_length  = l3; \
       name(&_s1, &_s2, &_s3); \
     } while(0)

#  define F77_PROTO_CCCCC(name) \
     name(struct dsc$descriptor_s*, struct dsc$descriptor_s*, \
          struct dsc$descriptor_s*, struct dsc$descriptor_s*, \
          struct dsc$descriptor_s*)
#  define F77_CALLS_CCCCC(name,c1,l1,c2,l2,c3,l3,c4,l4,c5,l5) \
     do { \
       struct dsc$descriptor_s _s1, _s2, _s3, _s4, _s5; \
       _s1.dsc$b_dtype   = DSC$K_DTYPE_T; \
       _s1.dsc$b_class   = DSC$K_CLASS_S; \
       _s1.dsc$a_pointer = c1; \
       _s1.dsc$w_length  = l1; \
       _s2.dsc$b_dtype   = DSC$K_DTYPE_T; \
       _s2.dsc$b_class   = DSC$K_CLASS_S; \
       _s2.dsc$a_pointer = c2; \
       _s2.dsc$w_length  = l2; \
       _s3.dsc$b_dtype   = DSC$K_DTYPE_T; \
       _s3.dsc$b_class   = DSC$K_CLASS_S; \
       _s3.dsc$a_pointer = c3; \
       _s3.dsc$w_length  = l3; \
       _s4.dsc$b_dtype   = DSC$K_DTYPE_T; \
       _s4.dsc$b_class   = DSC$K_CLASS_S; \
       _s4.dsc$a_pointer = c4; \
       _s4.dsc$w_length  = l4; \
       _s5.dsc$b_dtype   = DSC$K_DTYPE_T; \
       _s5.dsc$b_class   = DSC$K_CLASS_S; \
       _s5.dsc$a_pointer = c5; \
       _s5.dsc$w_length  = l5; \
       name(&_s1, &_s2, &_s3, &_s4, &_s5); \
     } while(0)

#  define F77_PROTO_CI(name) \
     name(struct dsc$descriptor_s*, INTEGER*)
#  define F77_CALLS_CI(name,c1,l1,i2) \
     do { \
       struct dsc$descriptor_s _s1; \
       _s1.dsc$b_dtype   = DSC$K_DTYPE_T; \
       _s1.dsc$b_class   = DSC$K_CLASS_S; \
       _s1.dsc$a_pointer = c1; \
       _s1.dsc$w_length  = l1; \
       name(&_s1, i2); \
     } while(0)

#  define F77_PROTO_CCCI(name) \
     name(struct dsc$descriptor_s*, struct dsc$descriptor_s*, \
          struct dsc$descriptor_s*, INTEGER*)
#  define F77_CALLS_CCCI(name,c1,l1,c2,l2,c3,l3,i4) \
     do { \
       struct dsc$descriptor_s _s1, _s2, _s3; \
       _s1.dsc$b_dtype   = DSC$K_DTYPE_T; \
       _s1.dsc$b_class   = DSC$K_CLASS_S; \
       _s1.dsc$a_pointer = c1; \
       _s1.dsc$w_length  = l1; \
       _s2.dsc$b_dtype   = DSC$K_DTYPE_T; \
       _s2.dsc$b_class   = DSC$K_CLASS_S; \
       _s2.dsc$a_pointer = c2; \
       _s2.dsc$w_length  = l2; \
       _s3.dsc$b_dtype   = DSC$K_DTYPE_T; \
       _s3.dsc$b_class   = DSC$K_CLASS_S; \
       _s3.dsc$a_pointer = c3; \
       _s3.dsc$w_length  = l3; \
       name(&_s1, &_s2, &_s3, i4); \
     } while(0)

#  define F77_PROTO_CPI(name) \
     name(struct dsc$descriptor_s*, void**, INTEGER*)
#  define F77_CALLS_CPI(name,c1,l1,p2,i3) \
     do { \
       struct dsc$descriptor_s _s1; \
       _s1.dsc$b_dtype   = DSC$K_DTYPE_T; \
       _s1.dsc$b_class   = DSC$K_CLASS_S; \
       _s1.dsc$a_pointer = c1; \
       _s1.dsc$w_length  = l1; \
       name(&_s1, p2, i3); \
     } while(0)

#  define F77_PROTO_CPII(name) \
     name(struct dsc$descriptor_s*, void**, INTEGER*, INTEGER*)
#  define F77_CALLS_CPII(name,c1,l1,p2,i3,i4) \
     do { \
       struct dsc$descriptor_s _s1; \
       _s1.dsc$b_dtype   = DSC$K_DTYPE_T; \
       _s1.dsc$b_class   = DSC$K_CLASS_S; \
       _s1.dsc$a_pointer = c1; \
       _s1.dsc$w_length  = l1; \
       name(&_s1, p2, i3, i4); \
     } while(0)

#  define F77_PROTO_EE(name) \
     name(void*, void*)
#  define F77_CALLS_EE(name,e1,e2) \
     name(e1, e2)

#  define F77_PROTO_I(name) \
     name(INTEGER*)
#  define F77_CALLS_I(name,i1) \
     name(i1)

#  define F77_PROTO_II(name) \
     name(INTEGER*, INTEGER*)
#  define F77_CALLS_II(name,i1,i2) \
     name(i1, i2)

#  define F77_PROTO_IDE(name) \
     name(INTEGER*, DBLPREC*, void*)
#  define F77_CALLS_IDE(name,i1,d2,e3) \
     name(i1, d2, e3)

#  define F77_PROTO_IIII(name) \
     name(INTEGER*, INTEGER*, INTEGER*, INTEGER*)
#  define F77_CALLS_IIII(name,i1,i2,i3,i4) \
     name(i1, i2, i3, i4)

#  define F77_PROTO_CIDI(name) \
     name(struct dsc$descriptor_s*, INTEGER*, DBLPREC*, INTEGER*)
#  define F77_CALLS_CIDI(name,c1,l1,i2,d3,i4) \
     do { \
       struct dsc$descriptor_s _s1; \
       _s1.dsc$b_dtype   = DSC$K_DTYPE_T; \
       _s1.dsc$b_class   = DSC$K_CLASS_S; \
       _s1.dsc$a_pointer = c1; \
       _s1.dsc$w_length  = l1; \
       name(&_s1, i2, d3, i4); \
     } while(0)

#  define F77_PROTO_ECIE(name) \
     name(void*, struct dsc$descriptor_s*, INTEGER*, void*)
#  define F77_CALLS_ECIE(name,e1,c2,l2,i3,e4) \
     do { \
       struct dsc$descriptor_s _s2; \
       _s2.dsc$b_dtype   = DSC$K_DTYPE_T; \
       _s2.dsc$b_class   = DSC$K_CLASS_S; \
       _s2.dsc$a_pointer = c2; \
       _s2.dsc$w_length  = l2; \
       name(e1, &_s2, i3, e4); \
     } while(0)

#  define F77_PROTO_IRCI(name) \
     name(INTEGER*, REAL*, struct dsc$descriptor_s*, INTEGER*)
#  define F77_CALLS_IRCI(name,i1,r2,c3,l3,i4) \
     do { \
       struct dsc$descriptor_s _s3; \
       _s3.dsc$b_dtype   = DSC$K_DTYPE_T; \
       _s3.dsc$b_class   = DSC$K_CLASS_S; \
       _s3.dsc$a_pointer = c3; \
       _s3.dsc$w_length  = l3; \
       name(i1, r2, &_s3, i4); \
     } while(0)

#  define F77_PROTO_IRRC(name) \
     name(INTEGER*, REAL*, REAL*, struct dsc$descriptor_s*)
#  define F77_CALLS_IRRC(name,i1,r2,r3,c4,l4) \
     do { \
       struct dsc$descriptor_s _s4; \
       _s4.dsc$b_dtype   = DSC$K_DTYPE_T; \
       _s4.dsc$b_class   = DSC$K_CLASS_S; \
       _s4.dsc$a_pointer = c4; \
       _s4.dsc$w_length  = l4; \
       name(i1, r2, r3, &_s4); \
     } while(0)

#  define F77_PROTO_IDDDD(name) \
     name(INTEGER*, DBLPREC*, DBLPREC*, DBLPREC*, DBLPREC*)
#  define F77_CALLS_IDDDD(name,i1,d2,d3,d4,d5) \
     name(i1, d2, d3, d4, d5)

#  define F77_PROTO_RRRRC(name) \
     name(REAL*, REAL*, REAL*, REAL*, struct dsc$descriptor_s*)
#  define F77_CALLS_RRRRC(name,r1,r2,r3,r4,c5,l5) \
     do { \
       struct dsc$descriptor_s _s5; \
       _s5.dsc$b_dtype   = DSC$K_DTYPE_T; \
       _s5.dsc$b_class   = DSC$K_CLASS_S; \
       _s5.dsc$a_pointer = c5; \
       _s5.dsc$w_length  = l5; \
       name(r1, r2, r3, r4, &_s5); \
     } while(0)

#  define F77_PROTO_DDDIII(name) \
     name(DBLPREC*, DBLPREC*, DBLPREC*, INTEGER*, INTEGER*, INTEGER*)
#  define F77_CALLS_DDDIII(name,d1,d2,d3,i4,i5,i6) \
     name(d1, d2, d3, i4, i5, i6)

#  define F77_PROTO_ECDIIE(name) \
     name(void*, struct dsc$descriptor_s*, \
          DBLPREC*, INTEGER*, INTEGER*, void*)
#  define F77_CALLS_ECDIIE(name,e1,c2,l2,d3,i4,i5,e6) \
     do { \
       struct dsc$descriptor_s _s2; \
       _s2.dsc$b_dtype   = DSC$K_DTYPE_T; \
       _s2.dsc$b_class   = DSC$K_CLASS_S; \
       _s2.dsc$a_pointer = c2; \
       _s2.dsc$w_length  = l2; \
       name(e1, &_s2, d3, i4, i5, e6); \
     } while(0)

#  define F77_PROTO_ICDDDDI(name) \
     name(INTEGER*, struct dsc$descriptor_s*, \
          DBLPREC*, DBLPREC*, DBLPREC*, DBLPREC*, INTEGER*)
#  define F77_CALLS_ICDDDDI(name,i1,c2,l2,d3,d4,d5,d6,i7) \
     do { \
       struct dsc$descriptor_s _s2; \
       _s2.dsc$b_dtype   = DSC$K_DTYPE_T; \
       _s2.dsc$b_class   = DSC$K_CLASS_S; \
       _s2.dsc$a_pointer = c2; \
       _s2.dsc$w_length  = l2; \
       name(i1, &_s2, d3, d4, d5, d6, i7); \
     } while(0)

#  define F77_PROTO_EIIIDDIE(name) \
     name(void*, INTEGER*, INTEGER*, INTEGER*, \
          DBLPREC*, DBLPREC*, INTEGER*, void*)
#  define F77_CALLS_EIIIDDIE(name,e1,i2,i3,i4,d5,d6,i7,e8) \
     name(e1, i2, i3, i4, d5, d6, i7, e8)

#  define F77_PROTO_ICIRRIRRII(name) \
     name(INTEGER*, struct dsc$descriptor_s*, INTEGER*, \
          REAL*, REAL*, INTEGER*, REAL*, REAL*, INTEGER*, INTEGER*)
#  define F77_CALLS_ICIRRIRRII(name,i1,c2,l2,i3,r4,r5,i6,r7,r8,i9,i10) \
     do { \
       struct dsc$descriptor_s _s2; \
       _s2.dsc$b_dtype   = DSC$K_DTYPE_T; \
       _s2.dsc$b_class   = DSC$K_CLASS_S; \
       _s2.dsc$a_pointer = c2; \
       _s2.dsc$w_length  = l2; \
       name(i1, &_s2, i3, r4, r5, i6, r7, r8, i9, i10); \
     } while(0)

#else

#  define F77_PROTO_(name) \
     name##_(void)
#  define F77_CALLS_(name) \
     name##_()

#  define F77_PROTO_C(name) \
     name##_(char*, int)
#  define F77_CALLS_C(name,c1,l1) \
     name##_(c1, l1)

#  define F77_PROTO_CC(name) \
     name##_(char*, char*, int, int)
#  define F77_CALLS_CC(name,c1,l1,c2,l2) \
     name##_(c1, c2, l1, l2)

#  define F77_PROTO_CCC(name) \
     name##_(char*, char*, char*, int, int, int)
#  define F77_CALLS_CCC(name,c1,l1,c2,l2,c3,l3) \
     name##_(c1, c2, c3, l1, l2, l3)

#  define F77_PROTO_CCCCC(name) \
     name##_(char*, char*, char*, char*, char*, int, int, int, int, int)
#  define F77_CALLS_CCCCC(name,c1,l1,c2,l2,c3,l3,c4,l4,c5,l5) \
     name##_(c1, c2, c3, c4, c5, l1, l2, l3, l4, l5)



#  define F77_PROTO_CI(name) \
     name##_(char*, INTEGER*, int)
#  define F77_CALLS_CI(name,c1,l1,i2) \
     name##_(c1, i2, l1)

#  define F77_PROTO_CCCI(name) \
     name##_(char*, char*, char*, INTEGER*, int, int, int)
#  define F77_CALLS_CCCI(name,c1,l1,c2,l2,c3,l3,i4) \
     name##_(c1, c2, c3, i4, l1, l2, l3)

#  define F77_PROTO_CPI(name) \
     name##_(char*, void **, INTEGER*, int)
#  define F77_CALLS_CPI(name,c1,l1,p2,i3) \
     name##_(c1, p2, i3, l1)

#  define F77_PROTO_CPII(name) \
     name##_(char*, void **, INTEGER*, INTEGER*, int)
#  define F77_CALLS_CPII(name,c1,l1,p2,i3,i4) \
     name##_(c1, p2, i3, i4, l1)

#  define F77_PROTO_EE(name) \
     name##_(void*, void*)
#  define F77_CALLS_EE(name,e1,e2) \
     name##_(e1, e2)

#  define F77_PROTO_I(name) \
     name##_(INTEGER*)
#  define F77_CALLS_I(name,i1) \
     name##_(i1)

#  define F77_PROTO_II(name) \
     name##_(INTEGER*, INTEGER*)
#  define F77_CALLS_II(name,i1,i2) \
     name##_(i1, i2)

#  define F77_PROTO_IDE(name) \
     name##_(INTEGER*, DBLPREC*, void*)
#  define F77_CALLS_IDE(name,i1,d2,e3) \
     name##_(i1, d2, e3)

#  define F77_PROTO_IIII(name) \
     name##_(INTEGER*, INTEGER*, INTEGER*, INTEGER*)
#  define F77_CALLS_IIII(name,i1,i2,i3,i4) \
     name##_(i1, i2, i3, i4)

#  define F77_PROTO_CIDI(name) \
     name##_(char*, INTEGER*, DBLPREC*, INTEGER*, int)
#  define F77_CALLS_CIDI(name,c1,l1,i2,d3,i4) \
     name##_(c1, i2, d3, i4, l1)

#  define F77_PROTO_ECIE(name) \
     name##_(void*, char*, INTEGER*, void*, int)
#  define F77_CALLS_ECIE(name,e1,c2,l2,i3,e4) \
     name##_(e1, c2, i3, e4, l2)

#  define F77_PROTO_IRCI(name) \
     name##_(INTEGER*, REAL*, char*, INTEGER*, int)
#  define F77_CALLS_IRCI(name,i1,r2,c3,l3,i4) \
     name##_(i1, r2, c3, i4, l3)

#  define F77_PROTO_IRRC(name) \
     name##_(INTEGER*, REAL*, REAL*, char*, int)
#  define F77_CALLS_IRRC(name,i1,r2,r3,c4,l4) \
     name##_(i1, r2, r3, c4, l4)

#  define F77_PROTO_IDDDD(name) \
     name##_(INTEGER*, DBLPREC*, DBLPREC*, DBLPREC*, DBLPREC*)
#  define F77_CALLS_IDDDD(name,i1,d2,d3,d4,d5) \
     name##_(i1, d2, d3, d4, d5)

#  define F77_PROTO_RRRRC(name) \
     name##_(REAL*, REAL*, REAL*, REAL*, char*, int)
#  define F77_CALLS_RRRRC(name,r1,r2,r3,r4,c5,l5) \
     name##_(r1, r2, r3, r4, c5, l5)

#  define F77_PROTO_DDDIII(name) \
     name##_(DBLPREC*, DBLPREC*, DBLPREC*, INTEGER*, INTEGER*, INTEGER*)
#  define F77_CALLS_DDDIII(name,d1,d2,d3,i4,i5,i6) \
     name##_(d1, d2, d3, i4, i5, i6)

#  define F77_PROTO_ECDIIE(name) \
     name##_(void*, char*, \
          DBLPREC*, INTEGER*, INTEGER*, void*, int)
#  define F77_CALLS_ECDIIE(name,e1,c2,l2,d3,i4,i5,e6) \
     name##_(e1, c2, d3, i4, i5, e6, l2)

#  define F77_PROTO_ICDDDDI(name) \
     name##_(INTEGER*, char*, \
          DBLPREC*, DBLPREC*, DBLPREC*, DBLPREC*, INTEGER*, int)
#  define F77_CALLS_ICDDDDI(name,i1,c2,l2,d3,d4,d5,d6,i7) \
     name##_(i1, c2, d3, d4, d5, d6, i7, l2)

#  define F77_PROTO_EIIIDDIE(name) \
     name##_(void*, INTEGER*, INTEGER*, INTEGER*, \
          DBLPREC*, DBLPREC*, INTEGER*, void*)
#  define F77_CALLS_EIIIDDIE(name,e1,i2,i3,i4,d5,d6,i7,e8) \
     name##_(e1, i2, i3, i4, d5, d6, i7, e8)

#  define F77_PROTO_ICIRRIRRII(name) \
     name##_(INTEGER*, char*, INTEGER*, \
          REAL*, REAL*, INTEGER*, REAL*, REAL*, INTEGER*, INTEGER*, int)
#  define F77_CALLS_ICIRRIRRII(name,i1,c2,l2,i3,r4,r5,i6,r7,r8,i9,i10) \
     name##_(i1, c2, i3, r4, r5, i6, r7, r8, i9, i10, l2)

#endif

    /***** fpanelf.F *****/
extern void F77_PROTO_CI(fpsetpnam);    /* set parameter name in hbook */
extern void F77_PROTO_CIDI(fpsavev);
extern void F77_PROTO_CCCCC(fpgetpnam);
extern void F77_PROTO_CPII(fpsetufcn);
extern void F77_PROTO_I(fpsetnpar);
extern void F77_PROTO_I(fpsetoptw);
extern void F77_PROTO_I(fpgetoptw);
extern void F77_PROTO_I(fpsetoptd);
extern void F77_PROTO_I(fpgetoptd);
extern void F77_PROTO_I(fpsetoptu);
extern void F77_PROTO_I(fpgetoptu);
extern void F77_PROTO_(fpmninit);

    /***** hfpanelf.F *****/
extern void F77_PROTO_II(fphsetrng);
extern void F77_PROTO_IIII(fphgetrng);
extern void F77_PROTO_CI(fphsetid);
extern void F77_PROTO_(fphgrinit);
extern void F77_PROTO_IDE(fphgrupdt);
extern void F77_PROTO_(fphplot);
extern void F77_PROTO_(fphgrend);
extern void F77_PROTO_I(fphgetfmod);
extern void F77_PROTO_CPI(fphsetfmod);

    /***** vfpanelf.F *****/
extern void F77_PROTO_(fpvgrinit);
extern void F77_PROTO_IDE(fpvgrupdt);
extern void F77_PROTO_(fpvplot);
extern void F77_PROTO_(fpvgrend);
extern void F77_PROTO_CCC(fpvgetnam);
extern void F77_PROTO_CI(fpvexist);
extern void F77_PROTO_CCCI(fpvset);
extern void F77_PROTO_I(fpvgetfmod);
extern void F77_PROTO_CPI(fpvsetfmod);

    /***** minuit subroutines *****/
extern void F77_PROTO_ECIE(mncomd);
extern void F77_PROTO_ICDDDDI(mnparm);
extern void F77_PROTO_IDDDD(mnerrs);
extern void F77_PROTO_EIIIDDIE(mncont);
extern void F77_PROTO_ICDDDDI(mnpout);
extern void F77_PROTO_DDDIII(mnstat);
extern void F77_PROTO_II(mninpu);
extern void F77_PROTO_EE(mnintr);
extern void F77_PROTO_ECDIIE(mnexcm);
extern void F77_PROTO_I(mnrset);

    /***** hbook subroutines *****/
extern void F77_PROTO_IRCI(hunpak);
extern void F77_PROTO_ICIRRIRRII(hgive);

    /***** higz/hplot subroutines *****/
extern void F77_PROTO_IRRC(igraph);
extern void F77_PROTO_RRRRC(hplfra);
extern void F77_PROTO_CC(hplax);
extern void F77_PROTO_(igterm);

    /***** kuip subroutines *****/
/*#include <packlib.h>*/
extern void km_warn_user(Widget parent, char *warning, char *command);
extern int km_confirm( Widget toplevel, char *message, int def_ans);
extern int ku_open(int, char *, char *);
extern void ku_write(int, char *);
extern int  ku_close(int);
extern void km_help_cb(Widget w, char *help_cmd, XtPointer call_data);
extern void km_display_sctext(char *, char *, int, int, int);
extern void km_reset_appShell(Widget shell);
extern void km_store_appShell(Widget, char *);

#ifdef __cplusplus
}  /* extern "C" */
#endif

