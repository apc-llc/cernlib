/*
 * $Id: style.c,v 1.1.1.1 1996/03/01 11:38:57 mclareni Exp $
 *
 * $Log: style.c,v $
 * Revision 1.1.1.1  1996/03/01 11:38:57  mclareni
 * Paw
 *
 */
#include "paw/pilot.h"
/*CMZ :  2.06/06 13/12/94  19.42.54  by  Fons Rademakers*/
/*-- Author :*/
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <X11/StringDefs.h>
#include <X11/Intrinsic.h>
#include <Xm/Xm.h>
#include <Xm/ToggleBG.h>
#include <Xm/Text.h>
#include <Xm/FileSB.h>
#include <Xm/Scale.h>

#include "hmotif/pawm.h"
#include "hmotif/axis.h"
#include "hmotif/style.h"
#include "hmotif/attstate.h"

#define MAX_X_ZONE          5
#define MAX_Y_ZONE          5

/* extern functions */
extern Widget create_styleSaveDialog(void);
extern Widget create_styleOpenDialog(void);
extern void unselect_buttons(int,int);
extern void show_label(int,int );
extern void show_orien(void);
extern char *get_attributes_geometry(char*);
extern char *get_attributes_attributes(char*);
extern char *get_attributes_color(char*);
extern char *get_attributes_hatchStyle(char*);
extern char *get_attributes_defineColor(char*);
extern char *get_attributes_fontSettings(char*);
extern char *get_attributes_generalAttribute(char*);
extern void put_attributes_geometry(char*,char*);
extern void put_attributes_attributes(char*,char*);
extern void put_attributes_color(char*,char*);
extern void put_attributes_hatchStyle(char*,char*);
extern void put_attributes_defineColor(char*,char*);
extern void put_attributes_fontSettings(char*,char*);
extern void put_attributes_generalAttribute(char*,char*);

/* extern widgets */
extern Widget ResetHistoPanel;
extern Widget FileToggleButtonGadget;
extern Widget DateToggleButtonGadget;

extern Widget widget_saveAsStyle;
extern Widget widget_openStyle;

extern Widget OpenstylePushButton;
extern Widget SavestylePushButton;
extern Widget SavestyleasPushButton;

extern Widget IdToggleButtonGadget;
extern Widget EntriesToggleButtonGadget;
extern Widget MeanToggleButtonGadget;
extern Widget RmsToggleButtonGadget;
extern Widget UnderToggleButtonGadget;
extern Widget OverToggleButtonGadget;
extern Widget AllchanToggleButtonGadget;

extern Widget ChiToggleButtonGadget;
extern Widget ErrorsToggleButtonGadget;
extern Widget ParametersToggleButtonGadget;

extern Widget HoriToggleButtonGadget;
extern Widget VertToggleButtonGadget;
extern Widget BothToggleButtonGadget;

extern Widget widget_file;
extern Widget TLToggleButtonGadget;
extern Widget BLToggleButtonGadget;
extern Widget TRToggleButtonGadget;
extern Widget BRToggleButtonGadget;

extern Widget widget_date;
extern Widget TLToggleButtonGadget1;
extern Widget BLToggleButtonGadget1;
extern Widget TRToggleButtonGadget1;
extern Widget BRToggleButtonGadget1;

extern Widget widget_axis;
extern Widget widget_fit;
extern Widget widget_stat;
extern Widget widget_zone;
extern Widget widget_angle;

extern Widget DivAxisform;
extern Widget AxisPrimaryText;
extern Widget AxisSecondaryText;
extern Widget AxisTertiaryText;
extern Widget AxisPrimaryScale;
extern Widget AxisSecondaryScale;
extern Widget AxisTertiaryScale;
extern Widget XAxisToggleButtonGadget;
extern Widget YAxisToggleButtonGadget;
extern Widget ZAxisToggleButtonGadget;
extern Widget NoOptimiToggleButtonGadget;
extern Widget gridtoggleButtonGadget;
extern Widget logtoggleButtonGadget;
extern Widget labelPushButtonGadget;
extern Widget orienPushButtonGadget;
extern Widget AxisApplyButton;
extern Widget AxisResetButton;
extern Widget AxisCloseButton;

extern Widget widget_orien;

extern Widget widget_label;
extern Widget labelText1;
extern Widget labelText2;
extern Widget labelText3;
extern Widget labelText4;
extern Widget labelText5;
extern Widget labelText6;
extern Widget labelText7;
extern Widget labelText8;
extern Widget labelText9;
extern Widget labeltoggleButtonGadget0;
extern Widget labeltoggleButtonGadget1;
extern Widget labeltoggleButtonGadget2;
extern Widget labeltoggleButtonGadget3;
extern Widget labeltoggleButtonGadget4;
extern Widget labeltoggleButtonGadget5;
extern Widget labeltoggleButtonGadget6;
extern Widget labeltoggleButtonGadget7;
extern Widget labeltoggleButtonGadget8;
extern Widget labeltoggleButtonGadget9;

extern Widget FitToggleButtonGadget;
extern Widget StatToggleButtonGadget;

extern Widget widget_geometry;
extern Widget widget_fontSettings;
extern Widget widget_color;
extern Widget widget_attributes;
extern Widget widget_generalAttributes;
extern Widget widget_hatchStyle;
extern Widget widget_defineColor;

extern Widget SavestyleasPushButton;

extern Widget AxisRanges;
extern Widget Angles;
extern Widget GeometryButton;
extern Widget FontButton;
extern Widget AttrButton;

extern Widget StyleName;

extern Widget AngleThetaText;
extern Widget AnglePhiText;

extern Boolean grid_active;

extern int old_x_zone ;
extern int old_y_zone;
extern Widget zone_buttons[MAX_X_ZONE][MAX_Y_ZONE];

/* global data */
Widget widget_saveAsStyle = (Widget)NULL;
Widget widget_openStyle = (Widget)NULL;
Style *style_root;
Style *current_style;

/* global functions */
void save_style(void);

/* local functions */
static void save_cb(Widget, MenuCbStruct *,
            XmFileSelectionBoxCallbackStruct *);
static void open_cb(Widget, MenuCbStruct *,
            XmFileSelectionBoxCallbackStruct *a);
static void openstyle(char *);
static void save_style_as(char *);
static void insert_default_values(Style*);
static Style *search_style( Style *,char*);
static Style *create_style(Style *, FILE*,char*);

/***********************************************************************
 *                                                                     *
 *   Create Save Style As panel.                                       *
 *                                                                     *
 ***********************************************************************/
void save_style_as_dialog(Widget w)
{
   MenuCbStruct *menu_item;
   XmString      xmmask;

   if (!widget_saveAsStyle) {

      widget_saveAsStyle = create_styleSaveDialog();

      menu_item = make_menuStruct(w, widget_saveAsStyle);
      InstallMwmCloseCallback(XtParent(widget_saveAsStyle),
                  (XtCallbackProc)cancel_cb, (XtPointer)menu_item);
      XtVaSetValues(widget_saveAsStyle, XmNuserData, menu_item, NULL);
      XtUnmanageChild(XmFileSelectionBoxGetChild(widget_saveAsStyle,
                    XmDIALOG_HELP_BUTTON));
      XtAddCallback(widget_saveAsStyle,XmNokCallback,
             (XtCallbackProc)save_cb, menu_item);
      XtAddCallback(widget_saveAsStyle,XmNcancelCallback,
             (XtCallbackProc)cancel_cb, menu_item);
   }

   XtVaGetValues(widget_saveAsStyle, XmNdirMask, &xmmask, NULL);
   XmFileSelectionDoSearch(widget_saveAsStyle, xmmask);

   XtSetSensitive(w, False);
   XtManageChild(widget_saveAsStyle);
}

/***********************************************************************
 *                                                                     *
 *   Create Open Style panel.                                          *
 *                                                                     *
 ***********************************************************************/
void open_style_dialog(Widget w)
{
   MenuCbStruct *menu_item;
   XmString      xmmask;

   if (!widget_openStyle) {

      widget_openStyle = create_styleOpenDialog();

      menu_item = make_menuStruct(w, widget_openStyle);
      InstallMwmCloseCallback(XtParent(widget_openStyle),
                  (XtCallbackProc)cancel_cb, (XtPointer)menu_item);
      XtVaSetValues(widget_openStyle, XmNuserData, menu_item, NULL);
      XtUnmanageChild(XmFileSelectionBoxGetChild(widget_openStyle,
                    XmDIALOG_HELP_BUTTON));
      XtAddCallback(widget_openStyle,XmNokCallback,
             (XtCallbackProc)open_cb, menu_item);
      XtAddCallback(widget_openStyle,XmNcancelCallback,
             (XtCallbackProc)cancel_cb, menu_item);
   }

   XtVaGetValues(widget_openStyle, XmNdirMask, &xmmask, NULL);
   XmFileSelectionDoSearch(widget_openStyle, xmmask);

   XtSetSensitive(w, False);
   XtManageChild(widget_openStyle);
}

/***********************************************************************
 *                                                                     *
 *   Unmanage dialog. This routine should be called by the             *
 *   popdown_cb routine of the parent.                                 *
 *                                                                     *
 ***********************************************************************/
void unmanage_style_dialogs(void)
{
   MenuCbStruct  *item;

   if (widget_openStyle) {
      XtVaGetValues(widget_openStyle, XmNuserData, &item, NULL);
      popdown_argument_dialog(item);
   }
   if (widget_saveAsStyle) {
      XtVaGetValues(widget_saveAsStyle, XmNuserData, &item, NULL);
      popdown_argument_dialog(item);
   }
}

/***********************************************************************
 *                                                                     *
 *    Get the style file name from the FSB ans save the style.         *
 *                                                                     *
 ***********************************************************************/
static void save_cb(Widget w, MenuCbStruct *item,
            XmFileSelectionBoxCallbackStruct *call_data)
{
   char     *file;

   file = extract_normal_string(call_data->value);
   if (strempty(file)) {
      km_warn_user(w, "You must give a file name.", "");
      XtFree(file);
      return;
   }
#ifdef vms
   if ( strchr( file, ':' ) == NULL && strchr( file, ']' ) == NULL ) {
#else
   if (*file != '/' && *file != '~') {
#endif
      /*
       * if it is not a directory, determine the full pathname
       * of the selection by concatenating it to the "dir" part
       */
      char *dir, *newfile;
      dir = extract_normal_string(call_data->dir);
      newfile = XtMalloc(strlen(dir) + strlen(file) + 1);
      sprintf(newfile, "%s%s", dir, file);
      XtFree(dir);
      XtFree(file);
      file = newfile;
   }
   popdown_argument_dialog(item);

   save_style_as(file);

   XtFree(file);
}

/***********************************************************************
 *                                                                     *
 *    Get the style file name from the FSB ans read the style.         *
 *                                                                     *
 ***********************************************************************/
static void open_cb(Widget w, MenuCbStruct *item,
            XmFileSelectionBoxCallbackStruct *call_data)
 {
   char     *file;

   file = extract_normal_string(call_data->value);
   if (strempty(file)) {
      km_warn_user(w, "You must give a file name.", "");
      XtFree(file);
      return;
   }
#ifdef vms
   if ( strchr( file, ':' ) == NULL && strchr( file, ']' ) == NULL ) {
#else
   if (*file != '/') {
#endif
      /*
       * if it is not a directory, determine the full pathname
       * of the selection by concatenating it to the "dir" part
       */
      char *dir, *newfile;
      dir = extract_normal_string(call_data->dir);
      newfile = XtMalloc(strlen(dir) + strlen(file) + 1);
      sprintf(newfile, "%s%s", dir, file);
      XtFree(dir);
      XtFree(file);
      file = newfile;
   }
   popdown_argument_dialog(item);

   openstyle(file);

   XtFree(file);
}

/************************************************************************
 *                                                                      *
 *    Create and initialize a new style struct.                         *
 *                                                                      *
 ************************************************************************/
static Style *create_style(Style *style, FILE *fp, char *name)
{
   char       line[MAX_CMD_LENGTH];
   char      *temp;
   Style     *p;
   int        i, ind, n, j, op;
   Boolean    overwrite;

   if (style) {
      overwrite = True;
      for (j = 0; j < 9; j++)
       XtFree(style->Label[j]);

      XtFree((char *)style->Col);
      p = style;
   } else {
      overwrite = False;
      p = (Style *) XtCalloc(1, sizeof(Style));
   }

   op = 0;

   if (p) {

      if (!overwrite) {
       p->Header = XtCalloc(strlen(name)+1, 1);
       strcpy(p->Header, name);
       insert_default_values(p);
      }

      while (fgets(line,MAX_CMD_LENGTH - 1, fp) != NULL )
      {

       if (!strncmp(line, "Set FILE",8))
           sscanf(&line[8],"%d",&p->File);

       if (!strncmp(line,"Option FILE",11))
          p->OFile = 1;

       if (!strncmp(line,"Option NFIL",11))
          p->OFile = 0;

       if (!strncmp(line, "Set DATE",8))
           sscanf(&line[8],"%d",&p->Date);

       if (!strncmp(line,"Option DATE",11))
          p->ODate = 1;

       if (!strncmp(line,"Option NDAT",11))
          p->ODate = 0;

       if (!strncmp(line,"Set FIT",7))
          sscanf(&line[8],"%s",p->Fit);

       if (!strncmp(line,"Option FIT",10))
          p->OFit = 1;

       if (!strncmp(line,"Option NFIT",11))
            p->OFit = 0;

       if (!strncmp(line,"Set STAT",8))
          sscanf(&line[8],"%s",p->Stat);

       if (!strncmp(line,"Option STAT",11))
          p->OStat = 1;

       if (!strncmp(line,"Option NSTA",11))
          p->OStat = 0;


       if (!strncmp(line,"Zone",4))
          sscanf(&line[5],"%d %d",&p->Zone[0],&p->Zone[1]);

       if (!strncmp(line,"Angle",5))
          sscanf(&line[6], "%f %f", &p->Angle[0], &p->Angle[1]);

       if (!strncmp(line,"LABEL",5)) {
          sscanf(&line[6],"%d", &ind);
          temp = km_strip(strchr(line+8,' '));
          p->Label[ind-1] = XtCalloc(strlen(temp)+1, 1);
          strcpy(p->Label[ind-1], temp);
          XtFree(temp);
       }
       if (!strncmp(line,"Set NDVX",8))
          sscanf(&line[9],"%s",p->Ndvx);

       if (!strncmp(line,"Set NDVY",8))
          sscanf(&line[9],"%s",p->Ndvy);

       if (!strncmp(line,"Set NDVZ",8))
          sscanf(&line[9],"%s",p->Ndvz);

       if (!strncmp(line,"Set YMGU",8))
          sscanf(&line[9],"%f",&p->Ymgu);

       if (!strncmp(line,"Set GSIZ",8))
          sscanf(&line[9],"%f",&p->Gsiz);

       if (!strncmp(line,"Set YGTI",8))
          sscanf(&line[9],"%f",&p->Ygti);

       if (!strncmp(line,"Set XMGR",8))
          sscanf(&line[9],"%f",&p->Xmgr);

       if (!strncmp(line,"Set XWIN",8))
          sscanf(&line[9],"%f",&p->Xwin);

       if (!strncmp(line,"Set XMGL",8))
          sscanf(&line[9],"%f",&p->Xmgl);

       if (!strncmp(line,"Set SMGU",8))
          sscanf(&line[9],"%f",&p->Smgu);

       if (!strncmp(line,"Set SMGR",8))
          sscanf(&line[9],"%f",&p->Smgr);

       if (!strncmp(line,"Set XLAB",8))
          sscanf(&line[9],"%f",&p->Xlab);

       if (!strncmp(line,"Set YWIN",8))
          sscanf(&line[9],"%f",&p->Ywin);

       if (!strncmp(line,"Set YSIZ",8))
          sscanf(&line[9],"%f",&p->Ysiz);

       if (!strncmp(line,"Set XSIZ",8))
          sscanf(&line[9],"%f",&p->Xsiz);

       if (!strncmp(line,"Set YTIC",8))
          sscanf(&line[9],"%f",&p->Ytic);

       if (!strncmp(line,"Set XVAL",8))
          sscanf(&line[9],"%f",&p->Xval);

       if (!strncmp(line,"Set VSIZ",8))
          sscanf(&line[9],"%f",&p->Vsiz);

       if (!strncmp(line,"Set YMGL",8))
          sscanf(&line[9],"%f",&p->Ymgl);

       if (!strncmp(line,"Set XTIC",8))
          sscanf(&line[9],"%f",&p->Xtic);

       if (!strncmp(line,"Set TSIZ",8))
          sscanf(&line[9],"%f",&p->Tsiz);

       if (!strncmp(line,"Set YHTI",8))
          sscanf(&line[9],"%f",&p->Yhti);

       if (!strncmp(line,"Set YLAB",8))
          sscanf(&line[9],"%f",&p->Ylab);

       if (!strncmp(line,"Set ASIZ",8))
          sscanf(&line[9],"%f",&p->Asiz);

       if (!strncmp(line,"Set BARO",8))
          sscanf(&line[9],"%f",&p->Baro);

       if (!strncmp(line,"Set BARW",8))
          sscanf(&line[9],"%f",&p->Barw);

       if (!strncmp(line,"Set CSIZ",8))
          sscanf(&line[9],"%f",&p->Csiz);

       if (!strncmp(line,"Set HMAX",8))
          sscanf(&line[9],"%f",&p->Hmax);

       if (!strncmp(line,"Set ERRX",8))
          sscanf(&line[9],"%f",&p->Errx);

       if (!strncmp(line,"Set YVAL",8))
          sscanf(&line[9],"%f",&p->Yval);

       if (!strncmp(line,"Set HCOL",8))
          sscanf(&line[9],"%f",&p->Hcol);

       if (!strncmp(line,"Set BCOL",8))
          sscanf(&line[9],"%f",&p->Bcol);

       if (!strncmp(line,"Set PCOL",8))
          sscanf(&line[9],"%f",&p->Pcol);

       if (!strncmp(line,"Set FCOL",8))
          sscanf(&line[9],"%f",&p->Fcol);

       if (!strncmp(line,"Set XCOL",8))
          sscanf(&line[9],"%f",&p->Xcol);

       if (!strncmp(line,"Set YCOL",8))
          sscanf(&line[9],"%f",&p->Ycol);

       if (!strncmp(line,"Igset MSCF",10))
          sscanf(&line[11],"%f",&p->Mscf);

       if (!strncmp(line,"Igset MTYP",10))
          sscanf(&line[11],"%d",&p->Mtyp);

       if (!strncmp(line,"Igset LTYP",10))
          sscanf(&line[11],"%d",&p->Ltyp);

       if (!strncmp(line,"Set HTYP",8))
          sscanf(&line[9],"%d",&p->Htyp);

       if (!strncmp(line,"Set BTYP",8))
          sscanf(&line[9],"%d",&p->Btyp);

       if (!strncmp(line,"Set PTYP",8))
          sscanf(&line[9],"%d",&p->Ptyp);

       if (!strncmp(line,"Set FTYP",8))
          sscanf(&line[9],"%d",&p->Ftyp);

       if (!strncmp(line,"Set DMOD",8))
          sscanf(&line[9],"%d",&p->Dmod);

       if (!strncmp(line,"Igset NCOL",10)) {
          sscanf(&line[11],"%d",&p->Ncol);
          p->Col = (Color*) XtCalloc(p->Ncol, sizeof(Color));
       }

       if (!strncmp(line,"Set BWID",8))
          sscanf(&line[9],"%d",&p->Bwid);

       if (!strncmp(line,"Set HWID",8))
          sscanf(&line[9],"%d",&p->Hwid);

       if (!strncmp(line,"Set PWID",8))
          sscanf(&line[9],"%d",&p->Pwid);

       if (!strncmp(line,"Set XWID",8))
          sscanf(&line[9],"%d",&p->Xwid);

       if (!strncmp(line,"Set YWID",8))
          sscanf(&line[9],"%d",&p->Ywid);

       if (!strncmp(line,"Set CFON",8))
          sscanf(&line[9],"%s",p->Cfon);

       if (!strncmp(line,"Set TFON",8))
          sscanf(&line[9],"%s",p->Tfon);

       if (!strncmp(line,"Set LFON",8))
          sscanf(&line[9],"%s",p->Lfon);

       if (!strncmp(line,"Set VFON",8))
          sscanf(&line[9],"%s",p->Vfon);

       if (!strncmp(line,"Set GFON",8))
          sscanf(&line[9],"%s",p->Gfon);

       if (!strncmp(line,"COLOR",5)) {
          sscanf(&line[6], "%d", &ind);
          sscanf(&line[6],"%d %f %f %f",&i ,&(p->Col[ind].red),
            &(p->Col[ind].green),&(p->Col[ind].bleu));
       }

       if (!strncmp(line,"atit",4))
          strcpy (p->Atit,&line[5]);

       if (!strncmp(line,"Option Utit",11))
           op = 1;

       if (!strncmp(line,"Title",5)) {
          if (op)
             sscanf(&line[6],"%s",p->Htit);
          else
             sscanf(&line[6],"%s",p->Ptit);
       }

       if (!strncmp(line,"Option LINX",11))
          p->Linx =1;

       if (!strncmp(line,"Option LINY",11))
          p->Liny =1;

       if (!strncmp(line,"Option LINZ",11))
          p->Linz =1;

       if (!strncmp(line,"Option LOGX",11))
          p->Linx =0;

       if (!strncmp(line,"Option LOGY",11))
          p->Liny =0;

       if (!strncmp(line,"Option LOGZ",11))
          p->Linz =0;

       if (!strncmp(line,"Set GRID",8))
          sscanf(&line[9],"%d",&p->Grid);

       if (!strncmp(line,"Option NGRI",11))
          p->Grid = 0;
      }
   }
   return(p);
}

/************************************************************************
 *                                                                      *
 *    Search a style struct in the linked list of style structs         *
 *                                                                      *
 ************************************************************************/
static Style* search_style(Style *w, char *name)
{
   Style *temp;

   temp = w;
   while (temp) {
      if (!strcmp(temp->Header,name))
       return (temp);
      temp = temp->link;
   }
   return((Style*) NULL);
}

/************************************************************************
 *                                                                      *
 *       Find or open a new style and set the different widgets.        *
 *                                                                      *
 ************************************************************************/
static void openstyle(char *name)
{
   FILE   *fp;
   Style  *style;
   char   *s, *s1, *str,*comstr;
   int     temp, x, y,temp1;
   float   t, p;
   char    tempx[MAX_NUMBER], tempy[MAX_NUMBER], tempz[MAX_NUMBER];
   XmAnyCallbackStruct    cbs1;
   XmString    xmstr;

   str = XtCalloc(MAX_CMD_LENGTH, 1);


   cbs1.reason = 0;
   cbs1.event = (XEvent *) NULL;
   XtCallCallbacks(ResetHistoPanel,XmNactivateCallback, &cbs1);

   style = search_style(style_root, name);
   if (!style) {
      if ((fp = fopen(name, "r"))) {
       style = create_style(style, fp, name);
       style->link = style_root;
       style_root = style;
       fclose(fp);
      } else {
       fprintf(stderr, "Cannot find or open the style %s\n", name);
       XtFree(str);
       return;
      }
   }
  current_style = style;

#ifdef vms
  s = strrchr(name, ']');
  if (!s)
      s = strrchr(name, ':');
#else
  s = strrchr(name, '/');
#endif
  s1 = XtCalloc(MAX_CMD_LENGTH, 1);
  sprintf(s1,"Current Style: %s", s+1);
  xmstr = XmStringCreate(s1, XmSTRING_DEFAULT_CHARSET);
  XtVaSetValues(StyleName, XmNlabelString, xmstr, NULL);
  XmStringFree(xmstr);
  XtFree(s1);

  sprintf(str,"Exec %s",name);
  execute_kuip_cmd(str);

  if (widget_file) {
     switch (style->File) {
       case 1:
       XmToggleButtonGadgetSetState(TLToggleButtonGadget,True,True);
       break;
      case 2:
       XmToggleButtonGadgetSetState(TRToggleButtonGadget,True,True);
       break;
      case 3:
       XmToggleButtonGadgetSetState(BLToggleButtonGadget,True,True);
       break;
      case 4:
       XmToggleButtonGadgetSetState(BRToggleButtonGadget,True,True);
       break;
   }
  }

  if (style->OFile)
     XmToggleButtonGadgetSetState(FileToggleButtonGadget,True,False);
  else
     XmToggleButtonGadgetSetState(FileToggleButtonGadget,False,False);

  if (widget_date) {
     switch (style->Date) {
       case 1:
       XmToggleButtonGadgetSetState(TLToggleButtonGadget1,True,True);
       break;
      case 2:
       XmToggleButtonGadgetSetState(TRToggleButtonGadget1,True,True);
       break;
      case 3:
       XmToggleButtonGadgetSetState(BLToggleButtonGadget1,True,True);
       break;
      case 4:
       XmToggleButtonGadgetSetState(BRToggleButtonGadget1,True,True);
       break;
   }
  }

  if(style->ODate)
     XmToggleButtonGadgetSetState(DateToggleButtonGadget,True,False);
  else
     XmToggleButtonGadgetSetState(DateToggleButtonGadget,False,False);

  if (widget_fit){
     if(!strempty(&style->Fit[0])) {
      sscanf(&style->Fit[0],"%1d",&temp);
         if(temp)
            XmToggleButtonGadgetSetState(ChiToggleButtonGadget,True,False);
         else
            XmToggleButtonGadgetSetState(ChiToggleButtonGadget,False,False);
     }
     if(!strempty(&style->Fit[1])) {
      sscanf(&style->Fit[1],"%1d",&temp);
         if(temp)
            XmToggleButtonGadgetSetState(ErrorsToggleButtonGadget,True,False);
         else
            XmToggleButtonGadgetSetState(ErrorsToggleButtonGadget,False,False);
     }
     if(!strempty(&style->Fit[2])) {
      sscanf(&style->Fit[2],"%1d",&temp);
         if(temp)
          XmToggleButtonGadgetSetState(ParametersToggleButtonGadget,True,False);
         else
         XmToggleButtonGadgetSetState(ParametersToggleButtonGadget,False,False);
     }
  }
  if (style->OFit)
     XmToggleButtonGadgetSetState(FitToggleButtonGadget,True,False);
  else
     XmToggleButtonGadgetSetState(FitToggleButtonGadget,False,False);

  if(style->OFile)
     XmToggleButtonGadgetSetState(FileToggleButtonGadget,True,False);
  else
     XmToggleButtonGadgetSetState(FileToggleButtonGadget,False,False);

  if(style->ODate)
     XmToggleButtonGadgetSetState(DateToggleButtonGadget,True,False);
  else
     XmToggleButtonGadgetSetState(DateToggleButtonGadget,False,False);

  if (widget_stat) {
     if(!strempty(&style->Stat[0])) {
      sscanf(&style->Stat[0],"%1d",&temp);
      if(temp)
         XmToggleButtonGadgetSetState(AllchanToggleButtonGadget,True,False);
      else
         XmToggleButtonGadgetSetState(AllchanToggleButtonGadget,False,False);
     }
     if(!strempty(&style->Stat[1])) {
      sscanf(&style->Stat[1],"%1d",&temp);
      if(temp)
         XmToggleButtonGadgetSetState(OverToggleButtonGadget,True,False);
      else
         XmToggleButtonGadgetSetState(OverToggleButtonGadget,False,False);
     }
     if(!strempty(&style->Stat[2])) {
      sscanf(&style->Stat[2],"%1d",&temp);
      if(temp)
         XmToggleButtonGadgetSetState(UnderToggleButtonGadget,True,False);
      else
         XmToggleButtonGadgetSetState(UnderToggleButtonGadget,False,False);
     }
     if(!strempty(&style->Stat[3])) {
      sscanf(&style->Stat[3],"%1d",&temp);
      if(temp)
         XmToggleButtonGadgetSetState(RmsToggleButtonGadget,True,False);
      else
         XmToggleButtonGadgetSetState(RmsToggleButtonGadget,False,False);
     }
     if(!strempty(&style->Stat[4])) {
      sscanf(&style->Stat[4],"%1d",&temp);
      if(temp)
         XmToggleButtonGadgetSetState(MeanToggleButtonGadget,True,False);
      else
         XmToggleButtonGadgetSetState(MeanToggleButtonGadget,False,False);
     }
     if(!strempty(&style->Stat[5])) {
      sscanf(&style->Stat[5],"%1d",&temp);
      if(temp)
         XmToggleButtonGadgetSetState(EntriesToggleButtonGadget,True,False);
      else
         XmToggleButtonGadgetSetState(EntriesToggleButtonGadget,False,False);
     }
     if(!strempty(&style->Stat[6])) {
      sscanf(&style->Stat[6],"%1d",&temp);
      if(temp)
         XmToggleButtonGadgetSetState(IdToggleButtonGadget,True,False);
      else
         XmToggleButtonGadgetSetState(IdToggleButtonGadget,False,False);
     }
  }
  if (style->OStat)
     XmToggleButtonGadgetSetState(StatToggleButtonGadget,True,False);
  else
     XmToggleButtonGadgetSetState(StatToggleButtonGadget,False,False);


  if (widget_zone) {
     unselect_buttons(old_x_zone, old_y_zone);
     for (x=0; x<style->Zone[0]; x++) {
      for (y=0; y<style->Zone[1]; y++)
         TogglePushButton(zone_buttons[x][y], True);
     }
     old_x_zone = x;
     old_y_zone = y;
  }
  if (widget_angle) {
     t = style->Angle[0];
     p = style->Angle[1];
     t = 90 -t;
     p = -90 - p;
     t =  60*(((t-60)/180) + 1);
     p =  60*(((p+120)/(-360)) +1);
     getang(&t,&p);
     sprintf(str,"%d",(int)t);
     XmTextSetString(AngleThetaText,str);
     sprintf(str,"%d",(int)p);
     XmTextSetString(AnglePhiText,str);
  }
  if (widget_label) {
     XmTextSetString(labelText1,style->Label[0]);
     XmTextSetString(labelText2,style->Label[1]);
     XmTextSetString(labelText3,style->Label[2]);
     XmTextSetString(labelText4,style->Label[3]);
     XmTextSetString(labelText5,style->Label[4]);
     XmTextSetString(labelText6,style->Label[5]);
     XmTextSetString(labelText7,style->Label[6]);
     XmTextSetString(labelText8,style->Label[7]);
     XmTextSetString(labelText9,style->Label[8]);
  }
  if (widget_axis) {
     if (style->Linx)
          axis.x_log = False;
     else
          axis.x_log = True;
     if (style->Liny)
          axis.y_log = False;
     else
          axis.y_log = True;
     if (style->Linz)
          axis.z_log = False;
     else
          axis.z_log = True;
     if(style->Grid) {
      switch(style->Grid) {
         case 3:
            axis.x_grid = axis.y_grid = 1;
            break;

         case 1003:
            axis.x_grid = 0;
            axis.y_grid = 1;
            break;

         case 2003:
            axis.x_grid = 1;
            axis.y_grid = 0;
            break;
      }
     }
     else
      axis.x_grid = axis.y_grid = False;
     if(!strncmp(&style->Ndvx[0],"-",1)) {
      axis.xop = 1;
      sscanf(&style->Ndvx[1], "%s", tempx);
     }
     else {
      axis.xop = False;
      sscanf(&style->Ndvx[0], "%s", tempx);
     }
     if(!strncmp(&style->Ndvy[0],"-",1)) {
      axis.yop = True;
      sscanf(&style->Ndvy[1], "%s", tempy);
     }
     else {
      axis.yop = False;
      sscanf(&style->Ndvy[0], "%s", tempy);
     }
     if(!strncmp(&style->Ndvz[0],"-",1)) {
      axis.zop = True;
      sscanf(&style->Ndvz[1], "%s", tempz);
     }
     else {
      axis.zop = False;
      sscanf(&style->Ndvz[0], "%s", tempz);
     }
     sprintf(str,"%c", tempx[6]);
     axis.xnl = atoi(str);
     sprintf(str,"%c", tempy[6]);
     axis.ynl = atoi(str);
     sprintf(str,"%c", tempx[7]);
     axis.xo  = atoi(str);
     sprintf(str,"%c", tempy[7]);
     axis.yo  = atoi(str);
     sprintf(str,"%c", tempx[0]);
     axis.xnt = atoi(str);
     sprintf(str,"%c", tempy[0]);
     axis.ynt = atoi(str);
     sprintf(str,"%c", tempz[0]);
     axis.znt = atoi(str);
     sprintf(str,"%c%c", tempx[1],tempx[2]);
     axis.xns = atoi(str);
     sprintf(str,"%c%c", tempy[1],tempy[2]);
     axis.yns = atoi(str);
     sprintf(str,"%c%c", tempz[1],tempz[2]);
     axis.zns = atoi(str);
     sprintf(str,"%c%c", tempx[3],tempx[4]);
     axis.xnp = atoi(str);
     sprintf(str,"%c%c", tempy[3],tempy[4]);
     axis.ynp = atoi(str);
     sprintf(str,"%c%c", tempz[3],tempz[4]);
     axis.znp = atoi(str);
     XmToggleButtonGadgetSetState(YAxisToggleButtonGadget,False,False);
     XmToggleButtonGadgetSetState(ZAxisToggleButtonGadget,False,False);
     XmToggleButtonGadgetSetState(XAxisToggleButtonGadget,True,False);
     sprintf(str,"%d",axis.xnp);
     XmTextSetString(AxisPrimaryText,str);
     sprintf(str,"%d",axis.xns);
     XmTextSetString(AxisSecondaryText,str);
     sprintf(str,"%d",axis.xnt);
     XmTextSetString(AxisTertiaryText,str);
     XmScaleSetValue(AxisPrimaryScale,axis.xnp);
     XmScaleSetValue(AxisSecondaryScale,axis.xns);
     XmScaleSetValue(AxisTertiaryScale,axis.xnt);
     XmToggleButtonGadgetSetState(NoOptimiToggleButtonGadget,axis.xop,False);
     XmToggleButtonGadgetSetState(logtoggleButtonGadget, axis.x_log,False);
     if (grid_active){
      XtSetSensitive(gridtoggleButtonGadget,True);
      XmToggleButtonGadgetSetState(gridtoggleButtonGadget, axis.x_grid,False);
     }

     if (widget_label && XtIsManaged(widget_label)) {
      show_label(axis.xnl,1);
      XtSetSensitive(labelPushButtonGadget,False);
     }
     else
       XtSetSensitive(labelPushButtonGadget,True);
     if (widget_orien && XtIsManaged(widget_orien)) {
      XtSetSensitive(orienPushButtonGadget,False);
      show_orien();
     }
     else
        XtSetSensitive(orienPushButtonGadget,True);
  }
  if (widget_geometry)
  {
     sprintf(str,"%f",style->Ymgu);
     put_attributes_geometry("YMGU", str);

     sprintf(str,"%f",style->Gsiz);
     put_attributes_geometry("GSIZ", str);

     sprintf(str,"%f",style->Ygti);
     put_attributes_geometry("YGTI", str);

     sprintf(str,"%f",style->Xmgr);
     put_attributes_geometry("XMGR", str);

     sprintf(str,"%f",style->Xwin);
     put_attributes_geometry("XWIN", str);

     sprintf(str,"%f",style->Xmgl);
     put_attributes_geometry("XMGL", str);

     sprintf(str,"%f",style->Smgu);
     put_attributes_geometry("SMGU", str);

     sprintf(str,"%f",style->Smgr);
     put_attributes_geometry("SMGR", str);

     sprintf(str,"%f",style->Xlab);
     put_attributes_geometry("XLAB", str);

     sprintf(str,"%f",style->Ywin);
     put_attributes_geometry("YWIN", str);

     sprintf(str,"%f",style->Ysiz);
     put_attributes_geometry("YSIZ", str);

     sprintf(str,"%f",style->Xsiz);
     put_attributes_geometry("XSIZ", str);

     sprintf(str,"%f",style->Ytic);
     put_attributes_geometry("YTIC", str);

     sprintf(str,"%f",style->Xval);
     put_attributes_geometry("XVAL", str);

     sprintf(str,"%f",style->Vsiz);
     put_attributes_geometry("VSIZ", str);

     sprintf(str,"%f",style->Ymgl);
     put_attributes_geometry("YMGL", str);

     sprintf(str,"%f",style->Xtic);
     put_attributes_geometry("XTIC", str);

     sprintf(str,"%f",style->Tsiz);
     put_attributes_geometry("TSIZ", str);

     sprintf(str,"%f",style->Yhti);
     put_attributes_geometry("YHTI", str);

     sprintf(str,"%f",style->Ylab);
     put_attributes_geometry("YLAB", str);

     sprintf(str,"%f",style->Asiz);
     put_attributes_geometry("ASIZ", str);

     sprintf(str,"%f",style->Baro);
     put_attributes_geometry("BARO", str);

     sprintf(str,"%f",style->Barw);
     put_attributes_geometry("BARW", str);

     sprintf(str,"%f",style->Csiz);
     put_attributes_geometry("CSIZ", str);

     sprintf(str,"%f",style->Hmax);
     put_attributes_geometry("HMAX", str);

     sprintf(str,"%f",style->Errx);
     put_attributes_geometry("ERRX", str);

     sprintf(str,"%f",style->Yval);
     put_attributes_geometry("YVAL", str);
  }
  if (widget_color) {

     sprintf(str,"%f",style->Hcol);
     put_attributes_color("HCOL", str);


     sprintf(str,"%f",style->Bcol);
     put_attributes_color("BCOL", str);

     sprintf(str,"%f",style->Pcol);
     put_attributes_color("PCOL", str);

     sprintf(str,"%f",style->Fcol);
     put_attributes_color("FCOL", str);

     sprintf(str,"%f",style->Xcol);
     put_attributes_color("XCOL", str);

     sprintf(str,"%f",style->Ycol);
     put_attributes_color("YCOL", str);

  }
  if (widget_hatchStyle) {

     sprintf(str,"%d",style->Htyp);
     put_attributes_hatchStyle("HTYP", str);

     sprintf(str,"%d",style->Ptyp);
     put_attributes_hatchStyle("PTYP", str);

     sprintf(str,"%d",style->Ftyp);
     put_attributes_hatchStyle("FTYP", str);

     sprintf(str,"%d",style->Btyp);
     put_attributes_hatchStyle("BTYP", str);
  }
  if (widget_generalAttributes) {

     sprintf(str,"%f",style->Mscf);
     put_attributes_generalAttribute("MSCF", str);

     sprintf(str,"%d",style->Mtyp);
     put_attributes_generalAttribute("MTYP", str);

     sprintf(str,"%d",style->Ltyp);
     put_attributes_generalAttribute("LTYP", str);


     sprintf(str,"%d",style->Dmod);
     put_attributes_generalAttribute("DMOD", str);

  }
  if (widget_fontSettings) {

     strcpy(str,style->Cfon);
     put_attributes_fontSettings("CFON", str);

     strcpy(str,style->Tfon);
     put_attributes_fontSettings("TFON", str);

     strcpy(str,style->Lfon);
     put_attributes_fontSettings("LFON", str);


     strcpy(str,style->Vfon);
     put_attributes_fontSettings("VFON", str);

     strcpy(str,style->Gfon);
     put_attributes_fontSettings("GFON", str);

  }

  if (widget_defineColor) {

      sprintf(str,"%d",style->Ncol);
      put_attributes_defineColor("NCOL", str);

     temp1 = style->Ncol - 1;
     while ( temp1>=0) {
      sprintf(str,"%d %f %f %f",temp1, style->Col[temp1].red,
         style->Col[temp1].green,style->Col[temp1].bleu);
      put_attributes_defineColor("COLOR",str);
      temp1--;
     }
  }

  if (widget_attributes) {

     strcpy(str,style->Ptit);
     put_attributes_attributes("PTIT",str);

     strcpy(str,style->Htit);
     put_attributes_attributes("HTIT",str);

     strcpy(str,style->Atit);
     put_attributes_attributes("ATIT",str);

     sprintf(str, "%d",style->Bwid);
     put_attributes_attributes("BWID",str);

     sprintf(str, "%d",style->Hwid);
     put_attributes_attributes("HWID",str);

     sprintf(str, "%d",style->Fwid);
     put_attributes_attributes("FWID",str);

     sprintf(str, "%d",style->Pwid);
     put_attributes_attributes("PWID",str);

     sprintf(str, "%d",style->Xwid);
     put_attributes_attributes("XWID",str);

     sprintf(str, "%d",style->Ywid);
     put_attributes_attributes("YWID",str);
  }
  plot_current_histo();
  XtFree(str);
}

/************************************************************************
 *                                                                      *
 *     Save the current style in a file.                                *
 *                                                                      *
 ************************************************************************/
static void save_style_as(char *name)
{
   FILE    *fp;
   int      fid;
   Style   *new_style;
   char    *theta, *phi, *s, *str,*comstr;
   float    t, p;
   int      num;

   char c[6];
   int n,m;
   float r;

    comstr = XtCalloc(MAX_CMD_LENGTH,1);
   if ((fp = fopen(name, "w")) == NULL) {
      fprintf(stderr, "Cannot open for writing the style file %s\n", name);
      return;
   }

   fprintf(fp,"Igset * ; Set * ; Option * ; Option ZFL1\n");

   Hplset("?FILE",&r);
   fprintf( fp, "Set FILE %d\n", (int)r);
   strcpy(c,"FILE");
   Gethplopt(c);
   if (!strcmp(c,"FILE"))
     fprintf(fp, "Option FILE \n");
   else
     fprintf(fp,"Option NFIL \n");

   Hplset("?DATE",&r);
   fprintf(fp, "Set DATE %d\n", (int)r);
   strcpy(c, "DATE");
   Gethplopt(c);
   if (!strcmp (c,"DATE"))
    fprintf( fp, "Option DATE \n");
   else
    fprintf(fp, "Option NDAT \n");

   Hplset("?FIT",&r);
   fprintf(fp,"Set FIT %.3d\n", (int)r);
   strcpy(c,"FIT ");
   Gethplopt(c);
   if (!strcmp (c,"FIT "))
     fprintf(fp,"Option FIT \n");
   else
     fprintf(fp,"Option NFIT \n");

    Hplset("?STAT",&r);
    if (r == 1111)
      fprintf(fp,"Set STAT %d%d%d%4d\n",0,0,0,(int)r);
    else
      fprintf(fp,"Set STAT %.7d\n",(int)r);
    strcpy(c,"STA ");
    Gethplopt(c);
    if (!strcmp (c,"STA "))
      fprintf(fp,"Option STAT \n");
    else
      fprintf(fp,"Option NSTA \n");

   hplgzo(&n,&m);
   fprintf(fp,"Zone %d %d\n", n, m);

   gettp(&t,&p);
   fprintf(fp,"Angle %f %f\n",t,p);

   Hplset("?NDVX",&r);
   if (r>0)
    fprintf(fp,"Set NDVX %5.2f\n", r);
   else
    fprintf(fp,"Set NDVX %5.2f\n", r);

   Hplset("?NDVY",&r);
   if (r>0)
    fprintf(fp,"Set NDVY %5.2f\n", r);
   else
    fprintf(fp,"Set NDVY %5.2f\n", r);

   Hplset("?NDVZ",&r);
   if (r>0)
    fprintf(fp,"Set NDVZ %5.2f\n", r);
   else
    fprintf(fp,"Set NDVZ %5.2f\n", r);

   strcpy(c,"GRID");
   Gethplopt(c);
   if (!strcmp(c,"NGRI"))
      fprintf(fp,"Option NGRI\n");
   else {
      Hplset("?GRID",&r);
      fprintf(fp,"Set GRID %d\n", (int)r);
      fprintf(fp,"Option GRID\n");
   }

   strcpy(c,"LOGX");
   Gethplopt(c);
   if (!strcmp(c,"LOGX"))
     fprintf(fp,"Option LOGX \n");
   else
     fprintf(fp,"Option LINX \n");

   strcpy(c,"LOGY");
   Gethplopt(c);
    if (!strcmp(c,"LOGY"))
     fprintf(fp,"Option LOGY \n");
   else
     fprintf(fp,"Option LINY \n");

   strcpy(c,"LOGZ");
   Gethplopt(c);
    if (!strcmp(c,"LOGZ"))
     fprintf(fp,"Option LOGZ \n");
   else
     fprintf(fp,"Option LINZ \n");

   strcpy(c,"TIC ");
   Gethplopt(c);
    if (!strcmp(c,"TIC "))
     fprintf(fp,"Option TIC  \n");
   else
     fprintf(fp,"Option NTIC \n");


   for (n=1;n<=9;n++) {
        num = 0;
        s = XtCalloc(MAX_CMD_LENGTH,1);
        Hplglb(&n,&m,s,MAX_CMD_LENGTH-1,&num);
        str = XtCalloc(num+1,1);
        strcpy(str,s);
        if (m!=0)
          fprintf(fp, "LABEL %d %d %s\n", n, m, str);
        XtFree(str);
        XtFree(s);
   }

/* geometry attributes */

     str=get_attributes_geometry("YMGU");
     if (str) fprintf(fp,"Set YMGU %s\n",str);
     XtFree(str);

     str=get_attributes_geometry("GSIZ");
     if(str) fprintf(fp,"Set GSIZ %s\n",str);
     XtFree(str);

     str=get_attributes_geometry("YGTI");
     if(str) fprintf(fp,"Set YGTI %s\n",str);
     XtFree(str);

     str=get_attributes_geometry("XMGR");
     if(str) fprintf(fp,"Set XMGR %s\n",str);
     XtFree(str);

     str=get_attributes_geometry("XWIN");
     if(str) fprintf(fp,"Set XWIN %s\n",str);
     XtFree(str);

     str=get_attributes_geometry("XMGL");
     if(str) fprintf(fp,"Set XMGL %s\n",str);
     XtFree(str);

     str=get_attributes_geometry("SMGU");
     if(str) fprintf(fp,"Set SMGU %s\n",str);
     XtFree(str);

     str=get_attributes_geometry("SMGR");
     if(str) fprintf(fp,"Set SMGR %s\n",str);
     XtFree(str);

     str=get_attributes_geometry("XLAB");
     if(str) fprintf(fp,"Set XLAB %s\n",str);
     XtFree(str);

     str=get_attributes_geometry("YWIN");
     if(str) fprintf(fp,"Set YWIN %s\n",str);
     XtFree(str);

     str=get_attributes_geometry("YSIZ");
     if(str) fprintf(fp,"Set YSIZ %s\n",str);
     XtFree(str);

     str=get_attributes_geometry("XSIZ");
     if( str) fprintf(fp,"Set XSIZ %s\n", str);
     XtFree(str);

     str=get_attributes_geometry("YTIC");
     if(str) fprintf(fp,"Set YTIC %s\n",str);
     XtFree(str);

     str=get_attributes_geometry("XVAL");
     if(str) fprintf(fp,"Set XVAL %s\n",str);
     XtFree(str);

     str=get_attributes_geometry("VSIZ");
     if(str) fprintf(fp,"Set VSIZ %s\n",str);
     XtFree(str);

     str=get_attributes_geometry("YMGL");
     if (str) fprintf(fp,"Set YMGL %s\n", str);
     XtFree(str);

     str=get_attributes_geometry("XTIC");
     if(str) fprintf(fp,"Set XTIC %s\n",str);
     XtFree(str);

     str=get_attributes_geometry("TSIZ");
     if(str) fprintf(fp,"Set TSIZ %s\n",str);
     XtFree(str);

     str=get_attributes_geometry("YHTI");
     if(str) fprintf(fp,"Set YHTI %s\n",str);
     XtFree(str);

     str=get_attributes_geometry("YLAB");
     if(str) fprintf(fp,"Set YLAB %s\n",str);
     XtFree(str);

     str=get_attributes_geometry("ASIZ");
     if(str) fprintf(fp,"Set ASIZ %s\n",str);
     XtFree(str);

     str=get_attributes_geometry("BARO");
     if(str) fprintf(fp,"Set BARO %s\n",str);
     XtFree(str);

     str=get_attributes_geometry("BARW");
     if(str) fprintf(fp,"Set BARW %s\n",str);
     XtFree(str);

     str=get_attributes_geometry("CSIZ");
     if(str) fprintf(fp,"Set CSIZ %s\n",str);
     XtFree(str);

     str=get_attributes_geometry("HMAX");
     if(str) fprintf(fp,"Set HMAX %s\n",str);
     XtFree(str);

     str=get_attributes_geometry("ERRX");
     if(str) fprintf(fp,"Set ERRX %s\n",str);
     XtFree(str);

     str=get_attributes_geometry("YVAL");
     if(str) fprintf(fp,"Set YVAL %s\n",str);
     XtFree(str);

/* color attributes */

      str=get_attributes_color("HCOL");
      if(str) fprintf(fp,"Set HCOL %s\n",str);
      XtFree(str);

      str=get_attributes_color("BCOL");
      if(str) fprintf(fp,"Set BCOL %s\n",str);
      XtFree(str);

      str=get_attributes_color("PCOL");
      if(str) fprintf(fp,"Set PCOL %s\n",str);
      XtFree(str);

      str=get_attributes_color("FCOL");
      if( str) fprintf(fp,"Set FCOL %s\n", str);
      XtFree(str);

      str=get_attributes_color("XCOL");
      if(str) fprintf(fp,"Set XCOL %s\n",str);
      XtFree(str);

      str=get_attributes_color("YCOL");
      if(str) fprintf(fp,"Set YCOL %s\n",str);
      XtFree(str);

/* hatch attributes */

      str=get_attributes_hatchStyle("HTYP");
      if(str) fprintf(fp,"Set HTYP %s\n",str);
      XtFree(str);

      str=get_attributes_hatchStyle("BTYP");
      if(str) fprintf(fp,"Set BTYP %s\n",str);
      XtFree(str);

      str=get_attributes_hatchStyle("PTYP");
      if(str) fprintf(fp,"Set PTYP %s\n",str);
      XtFree(str);

      str=get_attributes_hatchStyle("FTYP");
      if(str) fprintf(fp,"Set FTYP %s\n",str);
      XtFree(str);

/*generalAttributes attributes */

      str=get_attributes_generalAttribute("MSCF");
      if(str) fprintf(fp,"Igset MSCF %s\n",str);
      XtFree(str);

      str=get_attributes_generalAttribute("MTYP");
      if(str) fprintf(fp,"Igset MTYP %s\n",str);
      XtFree(str);

      str=get_attributes_generalAttribute("LTYP");
      if(str) fprintf(fp,"Igset LTYP %s\n",str);
      XtFree(str);

      str=get_attributes_generalAttribute("DMOD");
      if(str) fprintf(fp,"Set DMOD %s\n",str);
      XtFree(str);

/*fontSettings attributes */

      str =get_attributes_fontSettings("CFON");
      if (str) fprintf(fp,"Set CFON %s\n",str);
      XtFree(str);

      str =get_attributes_fontSettings("TFON");
      if (str) fprintf(fp,"Set TFON %s\n",str);
      XtFree(str);

      str =get_attributes_fontSettings("LFON");
      if (str) fprintf(fp,"Set LFON %s\n",str);
      XtFree(str);

      str =get_attributes_fontSettings("VFON");
      if (str) fprintf(fp,"Set VFON %s\n",str);
      XtFree(str);

      str =get_attributes_fontSettings("GFON");
      if (str) fprintf(fp,"Set GFON %s\n",str);
      XtFree(str);

   if (widget_defineColor) {
       str = get_attributes_defineColor("NCOL");
       if (str) {
        fprintf(fp,"Igset NCOL %s\n",str);
        num = atoi(str) - 1;
       }
       XtFree(str);
       while (num>=0) {
        sprintf(comstr,"COLOR %d",num);
        str = get_attributes_defineColor(comstr);
        if (str)
           fprintf(fp,"COLOR %d %s\n",num,str);
        XtFree(str);
        num--;
       }
    }

   if (widget_attributes) {
      str =get_attributes_attributes("PTIT");
      if (str[0] !='\0')
       fprintf(fp,"Title %s\n",str);
      XtFree(str);
      str =get_attributes_attributes("HTIT");
      if (str[0] !='\0') {
       fprintf(fp,"Option Utit\n");
       fprintf(fp,"Title %s U\n",str);
      }
      XtFree(str);
      str =get_attributes_attributes("ATIT");
      if (str[0] !='\0')
       fprintf(fp,"atit %s\n",str);
      XtFree(str);
   }

      str =get_attributes_attributes("BWID");
      if (str) fprintf(fp,"Set BWID %s\n",str);
      XtFree(str);

      str =get_attributes_attributes("HWID");
      if (str) fprintf(fp,"Set HWID %s\n",str);
      XtFree(str);

      str =get_attributes_attributes("FWID");
      if (str) fprintf(fp,"Set FWID %s\n",str);
      XtFree(str);

      str =get_attributes_attributes("PWID");
      if (str) fprintf(fp,"Set PWID %s\n", str);
      XtFree(str);

      str =get_attributes_attributes("XWID");
      if (str) fprintf(fp,"Set XWID %s\n",str);
      XtFree(str);

      str =get_attributes_attributes("YWID");
      if (str) fprintf(fp,"Set YWID %s\n",str);
      XtFree(str);

   XtFree(comstr);
   fclose(fp);

   fp= fopen(name,"r");

   new_style = search_style(style_root,name);
   if (!new_style) {
      new_style = create_style(new_style, fp, name);
      new_style->link = style_root;
      style_root = new_style;
   } else
      new_style = create_style(new_style, fp, "");

   fclose(fp);
}

/************************************************************************
 *                                                                      *
 *     Save the current style.                                          *
 *                                                                      *
 ************************************************************************/
void save_style()
{

   XmAnyCallbackStruct cbs1;

   if (current_style) {
      save_style_as(current_style->Header);
   }
   else {
      cbs1.reason = 0;
      cbs1.event = (XEvent *) NULL;
      XtCallCallbacks(SavestyleasPushButton,XmNactivateCallback, &cbs1);
   }
}

/************************************************************************
 *                                                                      *
 *     Define the default attributes values.                            *
 *                                                                      *
 ************************************************************************/
 static void insert_default_values(Style* p)
 {
    p->Ymgu = 2.00;
    p->Gsiz = 0.28;
    p->Ygti = 1.50;
    p->Xmgr = 2.00;
    p->Xwin = 2.00;
    p->Xmgl = 2.00;
    p->Smgu = 0.00;
    p->Smgr = 0.00;
    p->Xlab = 1.40;
    p->Ywin = 2.00;
    p->Ysiz = 20.00;
    p->Xsiz = 20.00;
    p->Ytic = 0.30;
    p->Xval = 0.40;
    p->Vsiz = 0.28;
    p->Ymgl = 2.00;
    p->Xtic = 0.30;
    p->Tsiz = 0.28;
    p->Yhti = 1.20;
    p->Ylab = 0.80;
    p->Asiz = 0.28;
    p->Baro = 0.25;
    p->Barw = 0.50;
    p->Csiz = 0.28;
    p->Hmax = 0.90;
    p->Errx = 0.5;
    p->Yval = 0.40;
    p->Hcol = 1.00;
    p->Bcol = 1.00;
    p->Pcol = 1;
    p->Fcol = 1;
    p->Xcol = 1;
    p->Ycol = 1;
    p->Htyp = 0;
    p->Btyp = 0;
    p->Ptyp = 0;
    p->Ftyp = 0;
    p->Mscf = 1.00;
    p->Mtyp = 1;
    p->Ltyp = 1;
    p->Dmod = 1;
    p->Ncol = 8;
    p->Bwid = 1;
    p->Hwid = 1;
    p->Fwid = 1;
    p->Pwid = 1;
    p->Xwid = 1;
    p->Ywid = 1;
    strcpy(p->Cfon,"2");
    strcpy(p->Tfon,"2");
    strcpy(p->Lfon,"2");
    strcpy(p->Vfon,"2");
    strcpy(p->Gfon,"2");
    p->Ptit[0] = '\0';
    p->Htit[0] = '\0';
    p->Atit[0] = '\0';
 }

