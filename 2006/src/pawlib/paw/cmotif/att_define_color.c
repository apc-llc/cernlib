/*
 * $Id: att_define_color.c,v 1.3 1999/03/01 09:07:01 couet Exp $
 *
 * $Log: att_define_color.c,v $
 * Revision 1.3  1999/03/01 09:07:01  couet
 * - previous commit was wrong
 *
 * Revision 1.2  1999/03/01 09:04:22  couet
 * - ERROR
 *
 * Revision 1.1.1.1  1996/03/01 11:38:53  mclareni
 * Paw
 *
 */
#include "paw/pilot.h"
/*CMZ :  2.04/15 05/03/94  14.53.28  by  Fons Rademakers*/
/*-- Author :*/
/*****************************************************************************
 *                                                                           *
 *    Manage the Define Color Dialog Box                                     *
 *                                                                           *
 *****************************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <Xm/Xm.h>
#include <X11/Xlib.h>
#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <Xm/Scale.h>
#include <Xm/Text.h>

#include "hmotif/uxxt.h"
#include "hmotif/pawm.h"
#include "hmotif/attstate.h"

#define NBCOLOR 2
#define NBCUE 8
#define rgb_space True

/* extern functions */
extern Widget  create_defineColorDialog();
extern void ixgetcol(int num,float *r,float *g,float *b);
extern void ChangeAttColor(XColor color,int num);
extern void UpdateAfterReset();

/* extern widgets */
extern Widget ObjectRow;
extern Widget HueScale,SaturationScale,LightnessScale,
   GreenScale,RedScale,BlueScale,
   GreenMax,GreenMin,BlueMax,BlueMin,RedMax,RedMin,SaturationMin,SaturationMax;
extern Widget ColorIndex,ResetColorBut,DefineColorBut,NcolText;
extern Widget DefineColorButton,CloseColorBut,ApplyColorBut,ResetColorButton;
extern Display *UxDisplay;
extern int UxScreen;
extern Widget widget_color,AttColorText;
extern Widget DefColorUpArrow,DefColorDownArrow,NcolUpArrow,NcolDownArrow;

/* global data */
Widget widget_defineColor=(Widget) NULL;

/* global function */
void show_defineColor (Widget w,caddr_t client_data,
                       XmAnyCallbackStruct *call_data);
void copy_color ( XColor *to,XColor *from);

/* local function */
static void rgb_to_hls ( XColor *color,int *hue,int *light,int *sat );
static void hls_to_rgb( int hue,int light,int sat,XColor * color );
static void put_color_cues ();
static void apply_color (int sender);
static void MoveScale_cb(Widget w,caddr_t client_data,
                         XmAnyCallbackStruct *call_data);
static void ChangeColor();
static void DefineColor_cb(Widget w,caddr_t client_data,
                           XmAnyCallbackStruct *call_data);
static void ResetColor_cb (Widget w,caddr_t client_data,
                           XmAnyCallbackStruct *call_data);
static void NewColor_cb (Widget w,caddr_t client_data,
                         XmAnyCallbackStruct *call_data);
static void NewNcol_cb (Widget w,caddr_t client_data,
                        XmAnyCallbackStruct *call_data);
static void ApplyColorDialog_cb(Widget w,caddr_t client_data,
                                XmAnyCallbackStruct *call_data);
static void CancelColorDialog_cb (Widget w,caddr_t client_data,
                                  XmAnyCallbackStruct *call_data);
static void ResetColorDialog_cb (Widget w,caddr_t client_data,
                                 XmAnyCallbackStruct *call_data);

/* local data */
static Colormap sys_cmap;
static int HueVal,SaturationVal,LightnessVal;
static XColor EditColor,InitColor;
static unsigned long ArrPixels[NBCOLOR+NBCUE];
static XColor cue_colors[NBCUE];
static Colormap OLDCOLORMAP; /***** MODIF ***/

/*****************************************************************************
 *                                                                           *
 *    Put attributes in widget_defineColor                                   *
 *                                                                           *
 *****************************************************************************/
void put_attributes_defineColor (char *label,char *value)
{
   int tempint;
   char *text;
   float red,green,blue;
   XmAnyCallbackStruct cb;

   if (!strcmp(label,"NCOL")) {
      /*   tempint=atoi(value); */
      XmTextSetString(NcolText,value);
   }
   if (!strcmp(label,"COLOR")) {
      sscanf(value,"%d %f %f %f",&tempint,&red,&green,&blue);
      /* if it is the current color update view */
      text = (char *) XmTextGetString(ColorIndex);
      if (tempint==atoi(text)) {
         cb.reason=0;
         cb.event=(XEvent *) NULL;
         XtCallCallbacks(ColorIndex,XmNactivateCallback,&cb);
      }
      XtFree(text);
   }
}

/*****************************************************************************
 *                                                                           *
 *   function which find the rgb number from an index using ixgetcol         *
 *                                                                           *
 *****************************************************************************/
void FindRgb(int Num,XColor *RgbColor)
{
   float Red,Green,Blue;

   ixgetcol(Num,&Red,&Green,&Blue);

   RgbColor->red=Red*65535;
   RgbColor->green=Green*65535;
   RgbColor->blue=Blue*65535;
   RgbColor->flags=DoRed | DoGreen | DoBlue;
}

/*****************************************************************************
 *                                                                           *
 *   give attributes values from widget_defineColor                          *
 *                                                                           *
 *****************************************************************************/
char *get_attributes_defineColor(char *label)
{
   char *temp;
   int colornum;
   XColor AskingColor;
   float red,green,blue;

   if (!strcmp(label,"NCOL")) {
      temp=(char *) XmTextGetString(NcolText);
   } else {
      sscanf(label,"COLOR %d",&colornum);
      FindRgb(colornum,&AskingColor);
      red=(float) AskingColor.red/65535;
      green=(float) AskingColor.green/65535;
      blue=(float) AskingColor.blue/65535;
      temp = XtCalloc(MAX_CMD_LENGTH,1);
      sprintf(temp,"%f %f %f",red,green,blue);
   }
   return temp;
   /* !!!!!!!!!! temp must be free by calling function */
}

/*****************************************************************************
 *                                                                           *
 *Copy the RGB components of one XColor to another.(first argument is target)*
 *                                                                           *
 *****************************************************************************/
void copy_color (to, from)
   XColor *to, *from;
{
   to->red = from->red;
   to->green = from->green;
   to->blue = from->blue;
}

/*****************************************************************************
 *                                                                           *
 *      Color conversions rgb to Hls                                         *
 *                                                                           *
 *****************************************************************************/
#define MAXIMUM(r, g, b)  ((r>g) ? ((r>b) ? (r):(b)) : ((g>b) ? (g):(b)))
#define MINIMUM(r, g, b)  ((r<g) ? ((r<b) ? (r):(b)) : ((g<b) ? (g):(b)))

static void rgb_to_hls (color,hue, light, sat)
   XColor *color;
   int *hue, *light, *sat;
{
   double max, min;
   double r = (double) color->red/65535;
   double g = (double) color->green/65535;
   double b = (double) color->blue/65535;
   double h, l, s;
   double rc, gc, bc;

   max = MAXIMUM(r, g, b);
   min = MINIMUM(r, g, b);
   l = (max + min)/2;

   if (max == min)
   {
      s = 0;
      h = 0;
   }
   else
   {
      if (l <= 0.5)
      {
         s = (max-min)/(max+min);
      }
      else
      {
         s = (max-min)/(2-max-min);
      }
      rc = (max-r)/(max-min);
      gc = (max-g)/(max-min);
      bc = (max-b)/(max-min);

      if (r == max)
         h = bc - gc;
      else if (g == max)
         h = 2 + rc - bc;
      else if (b == max)
         h = 4 + gc - rc;

      h = h*60;
      if (h < 0)
         h = h +360;
   }
   *hue = (unsigned short) (h);
   *light = (unsigned short) (l * 100);
   *sat = (unsigned short)  (s * 100);
}

/*****************************************************************************
 *                                                                           *
 *   Color conversion hls to rgb                                             *
 *                                                                           *
 *****************************************************************************/
#define VALUE(n1, n2, h)  ((h < 60) ? (n1 + (n2-n1)*h/60) : ((h<180) ? \
   (n2) : ((h<240) ? (n1+(n2-n1)*(240-h)/60) : (n1))))

static void hls_to_rgb(hue, light, sat, color)
   int hue, light, sat;
   XColor * color;
{
   double h = (double) hue;
   double l = (double) light/100;
   double s = (double) sat/100;
   double m1, m2;
   double r, g, b;

   if (l<= 0.5)
      m2 = l*(1 + s);
   else
      m2 = l + s - l*s;
   m1 = 2*l - m2;
   if ((s == 0) && (h == 0))
   {
      r = l; g = l; b = l;
   }
   else
   {
      r = VALUE(m1, m2, (((h+120) > 360) ? (h-240) : (h+120)));
      g = VALUE(m1, m2, h);
      b = VALUE(m1, m2, (((h-120) < 0) ? (h+240) : (h-120)));
   }
   color->red =  (int) (r * 65535);
   color->green = (int) (g * 65535);
   color->blue = (int) (b * 65535);
}

/*****************************************************************************
 *                                                                           *
 *       set all color cues to reflect the current edit_color.               *
 *                                                                           *
 *****************************************************************************/
static void put_color_cues ()
{
   hls_to_rgb (HueVal, LightnessVal, 99, &cue_colors[0]);

   hls_to_rgb (HueVal, LightnessVal,0, &cue_colors[1]);

   copy_color (&cue_colors[2], &EditColor);
   cue_colors[2].red = rgb_space ? 0 : 65535;

   copy_color (&cue_colors[3], &EditColor);
   cue_colors[3].red = rgb_space ? 65535 : 0;

   copy_color (&cue_colors[4],&EditColor);
   cue_colors[4].green = rgb_space ? 0 : 65535;

   copy_color (&cue_colors[5],&EditColor);
   cue_colors[5].green = rgb_space ? 65535 : 0;

   copy_color (&cue_colors[6], &EditColor);
   cue_colors[6].blue = rgb_space ? 0 : 65535;

   copy_color (&cue_colors[7],&EditColor );
   cue_colors[7].blue = rgb_space ? 65535 : 0;

   XStoreColors (UxDisplay,sys_cmap,cue_colors, 8);
}

/*****************************************************************************
 *                                                                           *
 *  adjust scrollbars and cues to reflect edit_color.                        *
 *                                                                           *
 *****************************************************************************/
static void apply_color (int sender)
{
   if (sender <= 10 ) { /* rgb have change */
      rgb_to_hls (&EditColor,  &HueVal, &LightnessVal, &SaturationVal);
      XmScaleSetValue(HueScale,HueVal);
      XmScaleSetValue(LightnessScale,LightnessVal);
      XmScaleSetValue(SaturationScale,SaturationVal);
   }
   if (sender >=10) { /* hls have change */
      XmScaleSetValue(RedScale,(int) EditColor.red);
      XmScaleSetValue(GreenScale,(int) EditColor.green);
      XmScaleSetValue(BlueScale,(int) EditColor.blue);
   }
   XStoreColor (UxDisplay, sys_cmap,&EditColor);
   put_color_cues ();
}

/*****************************************************************************
 *                                                                           *
 *      Manage mouvement of the colors scales                                *
 *                                                                           *
 *****************************************************************************/
static void MoveScale_cb(Widget w,caddr_t client_data,
                         XmAnyCallbackStruct *call_data)
{
   switch ((int ) client_data) {
      case 1 : /* RED */
         EditColor.red=((XmScaleCallbackStruct *) call_data)->value;
         break;

      case 2: /* Green */
         EditColor.green=((XmScaleCallbackStruct *) call_data)->value;
         break;

      case 3: /* Blue */
         EditColor.blue=((XmScaleCallbackStruct *) call_data)->value;
         break;

      case 11 : /* Hue */
         HueVal=((XmScaleCallbackStruct *) call_data)->value;
         hls_to_rgb(HueVal,LightnessVal,SaturationVal,&EditColor);
         break;

      case 12 : /* Saturation */
         SaturationVal=((XmScaleCallbackStruct *) call_data)->value;
         hls_to_rgb(HueVal,LightnessVal,SaturationVal,&EditColor);
         break;

      case 13 : /* Lightness */
         LightnessVal=((XmScaleCallbackStruct *) call_data)->value;
         hls_to_rgb(HueVal,LightnessVal,SaturationVal,&EditColor);
         break;

      default :
         fprintf(stderr,"Error in function MoveScale_cb");
   } /*end of switch */
   apply_color((int) client_data);
}

/*****************************************************************************
 *                                                                           *
 *      A new edit color                                                     *
 *                                                                           *
 *****************************************************************************/
static void ChangeColor()
{
   copy_color(&EditColor,&InitColor);
   apply_color(10); /* pour init */
}

/*****************************************************************************
 *                                                                           *
 * Update the init color with the current edit color : Define color button   *
 *                                                                           *
 *****************************************************************************/
static  void DefineColor_cb(Widget w,caddr_t client_data,
                            XmAnyCallbackStruct *call_data)
{
   char *ComStr;
   int NumCol,RVal,GVal,BVal;
   float red,green,blue;

   NumCol=atoi((char *) XmTextGetString(ColorIndex));

   XmScaleGetValue(RedScale,&RVal);
   XmScaleGetValue(GreenScale,&GVal);
   XmScaleGetValue(BlueScale,&BVal);

   ComStr=XtCalloc(MAX_CMD_LENGTH,1);
   red=(float) RVal/65535;
   blue=(float) BVal/65535;
   green=(float) GVal/65535;

   sprintf(ComStr,"COLOR %d %f %f %f",NumCol,red,green,blue);

   red=RVal/65535;
   blue=BVal/65535;
   green=GVal/65535;

   execute_kuip_cmd(ComStr);
   if (appres.auto_refresh) {
      plot_current_histo();
   }
   copy_color(&InitColor,&EditColor);
   /* To Change the Color of the Arrow in ColorSettings */
   if (widget_color)
      ChangeAttColor(EditColor,NumCol);
   XtFree(ComStr);
   /* storecolor update-view  et apply-init */
}

/*****************************************************************************
 *                                                                           *
 *  reset original color  (copy init to edit color) ; Reset Color Button     *
 *                                                                           *
 *****************************************************************************/
static  void ResetColor_cb (Widget w,caddr_t client_data,
                            XmAnyCallbackStruct *call_data)
{
   copy_color(&EditColor,&InitColor);
   apply_color(10);
}

/*****************************************************************************
 *                                                                           *
 *      A new color has been choosen                                         *
 *                                                                           *
 *****************************************************************************/
static  void NewColor_cb (Widget w,caddr_t client_data,
                          XmAnyCallbackStruct *call_data)
{
   int ColorNum;
   char SValue[5];
   int Ncol;
   Widget CurrentObject;
   TStateObject *data;
   char *ComStr;

   ColorNum=atoi((char *)XmTextGetString(ColorIndex));
   switch((int) client_data) {
      case 1: /* UpArrow */
         ColorNum++;
         break;
      case 2: /* DownArrow */
         ColorNum--;

      case 3: /* the text has been changed */
         break;
      default :
         fprintf(stderr,"Internal error in function NewColor_cb");
   }
   Ncol=atoi((char *) XmTextGetString(NcolText));
   if (ColorNum>=Ncol) {
      ColorNum=Ncol-1;
   }
   if (ColorNum<0) {
      ColorNum=0 ;
   }
   sprintf(SValue,"%d",ColorNum);
   XmTextSetString(ColorIndex,SValue);

   FindRgb(ColorNum,&InitColor);
   ChangeColor();
}

/*****************************************************************************
 *                                                                           *
 *    Manage the changement of NCOL                                          *
 *                                                                           *
 *****************************************************************************/
static  void NewNcol_cb (Widget w,caddr_t client_data,
                         XmAnyCallbackStruct *call_data)
{
   int Ncol;
   int ColorNum;
   char SValue[5];
   char *ComStr;
   int AttColor;
   XmAnyCallbackStruct cb;

   Ncol=atoi((char *)XmTextGetString(NcolText));
   switch((int) client_data) {
      case 1: /* UpArrow*/
         Ncol++;
         break;
      case 2: /* DownArrow */
         Ncol--;
         break;
      case 3: /* Text */
         break;
      default :
         fprintf(stderr,"Error in switch in NewNcol_cb ");
   }
   if (Ncol<8) {
      Ncol=8;
   }
   sprintf(SValue,"%d",Ncol);
   XmTextSetString(NcolText,SValue);
   ColorNum=atoi((char *) XmTextGetString(ColorIndex));
   if (ColorNum>=Ncol) {
      ColorNum=Ncol-1;
      sprintf(SValue,"%d",ColorNum);
      XmTextSetString(ColorIndex,SValue);

      /* the callback is going to update color of arrow */
      cb.reason=0;
      cb.event=(XEvent *)NULL;
       XtCallCallbacks(ColorIndex,XmNactivateCallback,&cb);
      /*
      XtCallCallbacks(AttColorText,XmNactivateCallback,&cb);
      */
   }

   ComStr=XtCalloc(MAX_CMD_LENGTH,1);
   sprintf(ComStr,"IGSET NCOL %d",Ncol);
   execute_kuip_cmd(ComStr);
   if ((appres.auto_refresh) && (call_data->reason != -999)) {
      plot_current_histo();
   }
   XtFree(ComStr);
}

/*****************************************************************************
 *                                                                           *
 *    Manage the Apply Button                                                *
 *                                                                           *
 *****************************************************************************/
static  void ApplyColorDialog_cb(Widget w,caddr_t client_data,
                                 XmAnyCallbackStruct *call_data)
{
   plot_current_histo();
}

/*****************************************************************************
 *                                                                           *
 *            Manage the Cancel button                                       *
 *                                                                           *
 *****************************************************************************/
static  void CancelColorDialog_cb (Widget w,caddr_t client_data,
                                   XmAnyCallbackStruct *call_data)
{
   XtUnmanageChild(widget_defineColor);
   XtSetSensitive(DefineColorButton,True);
}

/*****************************************************************************
 *                                                                           *
 *            Manage the reset button                                        *
 *                                                                           *
 *****************************************************************************/
static  void ResetColorDialog_cb (Widget w,caddr_t client_data,
                                  XmAnyCallbackStruct *call_data)
{
   XmAnyCallbackStruct cb;

   /*   sprintf(ComStr,"IGSET NCOL 8"); */
   /* the igset ncol order is doing by the callback */
   execute_kuip_cmd("COLOR 0 1 1 1");
   execute_kuip_cmd("COLOR 1 0 0 0");
   execute_kuip_cmd("COLOR 2 1 0 0");
   execute_kuip_cmd("COLOR 3 0 1 0");
   execute_kuip_cmd("COLOR 4 0 0 1");
   execute_kuip_cmd("COLOR 5 1 1 0");
   execute_kuip_cmd("COLOR 6 0 1 1");
   execute_kuip_cmd("COLOR 7 1 0 1");

   /*cb.reason=1; */ /* ou zero ??? */
   if (call_data->reason != -999)
      cb.reason = 1;
   else
      cb.reason = -999;
   cb.event=(XEvent *)NULL;


   XmTextSetString(ColorIndex,"0");
   XtCallCallbacks(ColorIndex,XmNactivateCallback,&cb);

   /* Reset Ncol */
   XmTextSetString(NcolText,"8");
   XtCallCallbacks(NcolText,XmNactivateCallback,&cb);

   if (widget_color)
      UpdateAfterReset();

   if ((appres.auto_refresh) && (call_data->reason != -999)){
      plot_current_histo();
   }
}

/*****************************************************************************
 *                                                                           *
 *     Init the Color Dialog Box and PopupInterface                          *
 *                                                                           *
 *****************************************************************************/
void show_defineColor (Widget w,caddr_t client_data,
                       XmAnyCallbackStruct *call_data)
{
   int i=0,j=0;
   static MenuCbStruct *menu_a_settings;
   XmAnyCallbackStruct cb;
   char temp[6];
   float currentValue;

   if (!widget_defineColor) {

      widget_defineColor= create_defineColorDialog();

      menu_a_settings = make_menuStruct(w, widget_defineColor);
      InstallMwmCloseCallback(XtParent(widget_defineColor),
                              (XtCallbackProc)cancel_cb,
                              (XtPointer)menu_a_settings);
      /* setup of colors */
      sys_cmap=DefaultColormap(UxDisplay,UxScreen);
      if ( ! XAllocColorCells(UxDisplay,sys_cmap,False,NULL,0,
                              ArrPixels,NBCOLOR+NBCUE)) {
         km_inform_user(XtParent(w),"Unable to allocate new \
colors: restart server or close the application using color","");
         widget_defineColor=0;
         return;
      }

      EditColor.pixel=ArrPixels[0];EditColor.flags=DoRed | DoBlue | DoGreen;
      InitColor.pixel=ArrPixels[1];InitColor.flags=DoRed | DoBlue | DoGreen;

      for (i=0;i<NBCUE;i++) {
         cue_colors[i].pixel=ArrPixels[i+NBCOLOR];
         cue_colors[i].flags=DoRed | DoBlue | DoGreen;
      }
      /* setup of pixels */
      /* main box */
      XtVaSetValues(widget_defineColor,XmNbackgroundPixmap,
                    XmUNSPECIFIED_PIXMAP,NULL);
      XtVaSetValues(DefColorUpArrow,XmNforeground,EditColor.pixel,NULL);
      XtVaSetValues(DefColorDownArrow,XmNforeground,EditColor.pixel,NULL);
      /* Cues marking the end of each scale */
      XtVaSetValues(SaturationMax,XmNbackground,cue_colors[0].pixel,NULL);
      XtVaSetValues(SaturationMin,XmNbackground,cue_colors[1].pixel,NULL);

      XtVaSetValues(RedMin,XmNbackground,cue_colors[2].pixel,NULL);
      XtVaSetValues(RedMax,XmNbackground,cue_colors[3].pixel,NULL);

      XtVaSetValues(GreenMin,XmNbackground,cue_colors[4].pixel,NULL);
      XtVaSetValues(GreenMax,XmNbackground,cue_colors[5].pixel,NULL);

      XtVaSetValues(BlueMin,XmNbackground,cue_colors[6].pixel,NULL);
      XtVaSetValues(BlueMax,XmNbackground,cue_colors[7].pixel,NULL);

      InitColor.pixel=ArrPixels[0];
      InitColor.flags=DoRed | DoGreen | DoBlue;

      XStoreColors(UxDisplay,sys_cmap,&InitColor,1);
/****
 * Callback
 ******/

      XtAddCallback(GreenScale,XmNdragCallback,
                    (XtCallbackProc) MoveScale_cb,(int *) 2);
      XtAddCallback(RedScale,XmNdragCallback,
                    (XtCallbackProc) MoveScale_cb,(int *) 1);
      XtAddCallback(BlueScale,XmNdragCallback,
                    (XtCallbackProc) MoveScale_cb,(int *) 3);

      XtAddCallback(SaturationScale,XmNdragCallback,
                    (XtCallbackProc) MoveScale_cb,(int *) 12);
      XtAddCallback(HueScale,XmNdragCallback,
                    (XtCallbackProc) MoveScale_cb,(int *) 11);
      XtAddCallback(LightnessScale,XmNdragCallback,
                    (XtCallbackProc) MoveScale_cb,(int *) 13);

      XtAddCallback(DefineColorBut,XmNactivateCallback,
                    (XtCallbackProc) DefineColor_cb,(int *) 13);
      XtAddCallback(ResetColorBut,XmNactivateCallback,
                    (XtCallbackProc) ResetColor_cb,(int *) 13);

      XtAddCallback(DefColorUpArrow,XmNarmCallback,
                    (XtCallbackProc) NewColor_cb,(int *) 1);
      XtAddCallback(DefColorDownArrow,XmNarmCallback,
                    (XtCallbackProc) NewColor_cb,(int *) 2);
      XtAddCallback(ColorIndex,XmNactivateCallback,
                    (XtCallbackProc) NewColor_cb,(int *) 3);
      XtAddCallback(ColorIndex,XmNlosingFocusCallback,
                    (XtCallbackProc) NewColor_cb,(int *) 3);

      XtAddCallback(NcolUpArrow,XmNarmCallback,
                    (XtCallbackProc) NewNcol_cb,(int *) 1);
      XtAddCallback(NcolDownArrow,XmNarmCallback,
                    (XtCallbackProc) NewNcol_cb,(int *) 2);
      XtAddCallback(NcolText,XmNactivateCallback,
                    (XtCallbackProc) NewNcol_cb,(int *) 3);

      XtAddCallback(CloseColorBut,XmNactivateCallback,
                    (XtCallbackProc) cancel_cb,menu_a_settings);
      XtAddCallback(ApplyColorBut,XmNactivateCallback,
                    (XtCallbackProc) ApplyColorDialog_cb,NULL);
      XtAddCallback(ResetColorButton,XmNactivateCallback,
                    (XtCallbackProc) ResetColorDialog_cb,NULL);
      if (appres.auto_refresh) XtSetSensitive(ApplyColorBut,False);
      else XtSetSensitive(ApplyColorBut,True);

      /* initiliaze Ncol */
      Igq("NCOL",&currentValue);
      sprintf(temp,"%3.0f",currentValue);
      XmTextSetString(NcolText,temp);

      /* Init Color Text To zero */
      XmTextSetString(ColorIndex,"0");
      cb.reason=0;
      cb.event=(XEvent *)NULL;

      XtCallCallbacks(ColorIndex,XmNactivateCallback,&cb);
   }
   XtSetSensitive(w,False);
   XtManageChild(widget_defineColor);
}

