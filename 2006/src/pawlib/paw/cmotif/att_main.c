/*
 * $Id: att_main.c,v 1.1.1.1 1996/03/01 11:38:54 mclareni Exp $
 *
 * $Log: att_main.c,v $
 * Revision 1.1.1.1  1996/03/01 11:38:54  mclareni
 * Paw
 *
 */
#include "paw/pilot.h"
/*CMZ :  2.05/01 22/03/94  15.55.05  by  Fons Rademakers*/
/*-- Author :*/
/*****************************************************************************
 *                                                                           *
 *   Main File to manage the attributes setting dialog and his children      *
 *   att_main.c                                                              *
 *                                                                           *
 *****************************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <Xm/Xm.h>
#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <Xm/Text.h>
#include <Xm/ToggleB.h>

#include "hmotif/uxxt.h"
#include "hmotif/pawm.h"
#include "hmotif/attstate.h"
/* This include file contain the declaration of  TStateObject */

#define NBOBJECT 6
/* There is 6 Object : Box, Histogramm, X Axis, Y Axis, Picture, Function */


/* extern functions */
extern Widget create_attributesFormDialog();
extern void show_color (Widget w,caddr_t client_data,
                        XmAnyCallbackStruct *call_data);
extern void show_defineColor (Widget w,caddr_t client_data,
                        XmAnyCallbackStruct *call_data);
extern void show_hatchStyle (Widget w);
extern void AddZero(char *string);

/* extern widgets */
extern Widget FontDialog,FontRow;
extern Widget widget_hatchStyle;
extern Widget ColorScale;
extern Widget HatchStyleButton,ColorButton,MarkerMenu,LineMenu;
extern Widget ApplyAttributesBut,ResetAttributesBut,CloseAttributesBut;
extern Widget ObjectRow,HistoToggle,BoxToggle,YAxisToggle,XAxisToggle,
              FunctionToggle,PictureToggle;
extern Widget IndexText;
extern Widget LineWidthUpArrow,LineWidthDownArrow,LineWidthLabel,
              LineWidthValueLabel;
extern Widget AttributesButton;
extern Widget DefineColorButton;
extern Widget widget_color;
extern Widget SurfaceToggle,ContourToggle,StatToggle,ZoneToggle;
extern Widget ResetObjectBut;
extern Widget ResetHatchBut,ResetAttColorButton;
extern Widget TitleForm,TitleText,TitleLabel;

/* global data */
Widget widget_attributes = (Widget) NULL;

/* global function */

/* local function */
static void LineWidthScale_cb(Widget w,caddr_t client_data,
                              XmAnyCallbackStruct *call_data);
static void InitArrStateObject();
static void ObjectChange_cb(Widget w,caddr_t client_data,
                              XmAnyCallbackStruct *call_data);
static void ApplyAttributesDialog_cb (Widget w,caddr_t client_data,
                              XmAnyCallbackStruct *call_data);
static void TitleText_cb (Widget w,caddr_t client_data,
                              XmAnyCallbackStruct *call_data);

/* local data */
/* static char *HistoChar[]="H"; */
static TStateObject StateObject[NBOBJECT];
static int Change;

/*****************************************************************************
 *                                                                           *
 *   Put Attributes value from widget_hatchStyle                             *
 *                                                                           *
 *****************************************************************************/
void put_attributes_hatchStyle(char *label,char *value)
{
   int i;
   char temp[6];
   Widget CurrentObject;
   TStateObject *data;
   XmAnyCallbackStruct cb;

   for (i=0;(i<NBOBJECT) && (*label!=*StateObject[i].FirstLetter);i++);
   StateObject[i].IndexHatch=atoi(value);


   /* update view */
   if (widget_hatchStyle) {
      XtVaGetValues(ObjectRow,XmNmenuHistory,&CurrentObject,NULL);
      XtVaGetValues(CurrentObject,XmNuserData,&data,NULL);
      /*sprintf(temp,"%d",data->IndexHatch);*/
      sprintf(temp,"%d",StateObject[i].IndexHatch);
      AddZero(temp);
      XmTextSetString(IndexText,temp);
      cb.reason=0;
      cb.event=(XEvent*)NULL;
      XtCallCallbacks(IndexText,XmNactivateCallback,&cb);
   }
}

/*****************************************************************************
 *                                                                           *
 *   Get Attributes value from widget_hatchStyle                             *
 *                                                                           *
 *****************************************************************************/
char *get_attributes_hatchStyle(char *label)
{
   char *temp;
   float r;

   temp=XtCalloc(10,1);
   strcpy(temp,"?");
   strcat(temp,label);
   Hplset(temp,&r);
   sprintf(temp,"%d",(int)r);
   return temp;
}

/*****************************************************************************
 *                                                                           *
 *   Put Attributes value from widget_color                                  *
 *                                                                           *
 *****************************************************************************/
void put_attributes_color(char *label,char *value)
{
   int i;
   XmAnyCallbackStruct cb;
   int pos;
   char tempstr[6];
   int tempint;

   for (i=0;(i<NBOBJECT) && (*label!=*StateObject[i].FirstLetter);i++);

   switch (*label) {
      case 'B' :
         pos=(int) strchr(value,'.')-(int) value;
         strncpy(tempstr,value,pos);
         tempint=atoi(tempstr);
         if (tempint>=1000) {
            StateObject[i].ContourColor=0;
            StateObject[i].SurfaceColor=tempint-1000;
         } else {
            StateObject[i].ContourColor=tempint;
            StateObject[i].SurfaceColor=0;
         }
         value=(char *)((int) value+pos+1);
         strncpy(tempstr,value,2);
         StateObject[i].StatColor=tempstr[1]-'0';
         StateObject[i].ZoneColor=tempstr[0]-'0';
         break;
      case 'H' :
      case 'X' :
      case 'Y' :
      case 'P' :
      case 'F' :
         tempint=atoi(value);
         if (tempint>=1000) {
            StateObject[i].ContourColor=0;
            StateObject[i].SurfaceColor=tempint-1000;
         } else {
            StateObject[i].ContourColor=tempint;
            StateObject[i].SurfaceColor=0;
         }
         break;
      default :
         fprintf(stderr,"unkown object in put_attributes_color");

   }
   cb.reason=0;
   cb.event=(XEvent *) NULL;
   XmToggleButtonSetState(BoxToggle,True,True);
   XtCallCallbacks(BoxToggle,XmNarmCallback,&cb); /*non ha nessun senso*/
}

/*****************************************************************************
 *                                                                           *
 *   Get Attributes value from widget_color                                  *
 *   WARNING: temp must be free by calling function                          *
 *                                                                           *
 *****************************************************************************/
char *get_attributes_color(char *label)
{
   char *temp;
   float r;

   temp=XtCalloc(10,1);

   strcpy(temp,"?");
   strcat (temp,label);
   Hplset(temp,&r);
   sprintf(temp, "%d", (int) r);
   return temp;
}

/*****************************************************************************
 *                                                                           *
 *   Get Attributes value from widget_attributes                             *
 *      LineWidth and Title                                                  *
 *****************************************************************************/
char *get_attributes_attributes(char *label)
{
   int i;
   char *temp;
   int tempint;
   float r;

   temp=XtCalloc(10,1); /* 505 = 2*MAXTitlezize + 5 */
   if(label[1]=='W') {
      strcpy(temp,"?");
      strcat(temp,label);
      Hplset(temp,&r);
      sprintf(temp,"%d",(int)r);
  }
  else {
    for (i=0;(i<NBOBJECT) && (*label!=*StateObject[i].FirstLetter);i++);
    if (*label=='A') {
            /* Special for X and Y Axis because style.c want the 2 attributes
               in one call */
            XtFree(temp);
            tempint=strlen(histogram.XAxisTitle) +
                    strlen(histogram.YAxisTitle) + 6 + 2;
            temp=XtCalloc(tempint,1); /* +2 if str empty -->
                                         space = 1character */
            if ((histogram.XAxisTitle) || (histogram.YAxisTitle)) {
               if (!histogram.XAxisTitle) {
                  sprintf(temp,"' ' '%s'", histogram.YAxisTitle);
               } else if (!histogram.YAxisTitle) {
                  sprintf(temp,"'%s' ' '" ,histogram.XAxisTitle);
               } else
                  sprintf(temp,"'%s' '%s'", histogram.XAxisTitle,
                          histogram.YAxisTitle);
            }
            else
               sprintf(temp,"' ' ' '");
         } else {
            sprintf(temp,"'%s'",StateObject[i].Title);
         }
   }
   return temp;
   /* !!!!!! temp must be free by calling function !!!!!!!!!!!!!*/
}

/*****************************************************************************
 *                                                                           *
 *   Put Attributes value from widget_attributes                             *
 *                                                                           *
 *****************************************************************************/
void put_attributes_attributes(char *label,char *value)
{
   int i,j;
   XmAnyCallbackStruct cb;
   char space;
   char apos;

   for (i=0;(i<NBOBJECT) && (*label!=*StateObject[i].FirstLetter);i++);
   switch (label[1]) {
      case 'W' :
         StateObject[i].LineWidth=atoi(value);
         break;
      case 'T' :
         if (*label=='A') {
            /* Special for X and Y Axis because style.c give 2 attributes
               in one call */
            sscanf(value,"%c%[^']%c%c%c%[^']%c",&apos,
                   StateObject[3].Title,&apos,&space,&apos,
                   StateObject[2].Title,&apos);
            if (!strcmp(StateObject[3].Title," ")) {
               histogram.XAxisTitle=NULL;
               strcpy(StateObject[3].Title,'\0');
            }
            else histogram.XAxisTitle=StateObject[3].Title;

            if (!strcmp(StateObject[2].Title," ")) {
               histogram.YAxisTitle=NULL;
               strcpy(StateObject[2].Title,'\0');
            }
            else histogram.YAxisTitle=StateObject[2].Title;

         }
         else {
         if (strcmp(value,"")) {
             j=1;
              while(value[j] !='\0' && j) {
                  if (value[j] == '\'' && value[j+1] == '\0')
                       j=0;
                 else {
                        StateObject[i].Title[j-1] = value[j];
                        j++;
                       }
              }
          }
      else
           strcpy(StateObject[i].Title,value);

      }
    break;
   }

   cb.reason=0;
   cb.event=(XEvent *) NULL;
   XmToggleButtonSetState(BoxToggle,True,True);
   XtCallCallbacks(BoxToggle,XmNarmCallback,&cb); /* non ha nessun senso*/
}

/*****************************************************************************
 *                                                                           *
 *    Remplace ' by @' in the title                                          *
 *                                                                           *
 *****************************************************************************/
static char *Clean(char *string)
{
   char *Apos;
   int pos;
   char end[MAX_TITLE_SIZE];

   Apos=strrchr(string,39); /* #27 == ' */
   if (Apos) {
      pos=((int)Apos  - (int)string  );
      strcpy(end,++Apos);
      string[pos]='\0';
      strcat(Clean(string),"@'");
      strcat(string,end);
   }
   return string;
}

/*****************************************************************************
 *                                                                           *
 *   Manage the Title text Widget                                            *
 *                                                                           *
 *****************************************************************************/
static void TitleText_cb (Widget w,caddr_t client_data,
                          XmAnyCallbackStruct *call_data)
{
   Widget CurrentToggle;
   TStateObject *data;
   char *ComStr;
   char Title[MAX_TITLE_SIZE];

   XtVaGetValues(ObjectRow,XmNmenuHistory,&CurrentToggle,NULL);
   XtVaGetValues(CurrentToggle,XmNuserData,&data,NULL);

   strcpy(Title,(char *)XmTextGetString(TitleText));
   strcpy(Title,Clean(Title));
   if (!strlen(Title)) {
      /* Title is empty */
      Title[0]='\0';
   }
   if (strcmp(Title,data->Title) )
   {
      Change=True;
      strcpy(data->Title,Title);
   }
   else  Change=False;
   ComStr=XtCalloc(MAX_CMD_LENGTH,1);
   if (Change) {
      if (CurrentToggle==PictureToggle) {
         if (data->Title[0]=='\0') {
            execute_kuip_cmd("TITLE");
         } else {
            sprintf(ComStr,"Title '%s'",data->Title);
         }
      }
      else {
         if (CurrentToggle==HistoToggle)
         {
            if (data->Title[0]=='\0') {
               execute_kuip_cmd("OPT HTIT");
            } else {
               execute_kuip_cmd("Opt Utit");
               sprintf(ComStr,"Title '%s' U",data->Title);
            }
         }
         else {
            if (CurrentToggle==XAxisToggle)
            {
               if (data->Title[0]=='\0') {
                  histogram.XAxisTitle=NULL;
               } else {
                  histogram.XAxisTitle=data->Title;
               }
            }
            else {
               if  (CurrentToggle==YAxisToggle)
               {
                  if (data->Title[0]=='\0') {
                     histogram.YAxisTitle=NULL;
                  } else {
                     histogram.YAxisTitle=data->Title;
                  }
               }
               else fprintf(stderr,"Unknown title in TitleText_cb");
            }
         }
      }
      execute_kuip_cmd(ComStr);
      if (appres.auto_refresh) {
         plot_current_histo();
      }
   } /* change */

   XtFree(ComStr);
}

/*****************************************************************************
 *                                                                           *
 *     Reset Current Object : Manage the ResetObjectBut                      *
 *     This Function Reset only one Object                                   *
 *                                                                           *
 *****************************************************************************/
static  void ResetObject_cb (Widget w,caddr_t client_data,
                             XmAnyCallbackStruct *call_data)
{
   XmAnyCallbackStruct cb;
   int InverseAuto;
   Widget CurrentToggle;
   TStateObject *CurrentState;
   char *ComStr;

   cb.reason=0;
   cb.event=(XEvent *)NULL;
   if (appres.auto_refresh) {
      InverseAuto         = True;
      appres.auto_refresh = !appres.auto_refresh;
   } else {
      InverseAuto = False;
   }

   if (widget_hatchStyle)
   {
      XtCallCallbacks(ResetHatchBut,XmNactivateCallback,&cb);
   }

   if (widget_color) {
      XtCallCallbacks(ResetAttColorButton,XmNactivateCallback,&cb);
   }

   XtVaGetValues(ObjectRow,XmNmenuHistory,&CurrentToggle,NULL);
   XtVaGetValues(CurrentToggle,XmNuserData,&CurrentState,NULL);

   /* Line Width */
   CurrentState->LineWidth=1;
   XtVaSetValues(LineWidthValueLabel,XmNlabelString,
                 XmStringCreateLtoR("1",XmSTRING_DEFAULT_CHARSET),NULL);
   ComStr=XtCalloc(MAX_CMD_LENGTH,1);
   sprintf(ComStr,"SET %sWID  %d",CurrentState->FirstLetter,1);
   execute_kuip_cmd(ComStr);

   /* Title Reset */
   CurrentState->Title[0]='\0';
   XmTextSetString(TitleText,CurrentState->Title);
   if (CurrentToggle==HistoToggle) {
      execute_kuip_cmd("OPT HTIT");
   } else {
      if (CurrentToggle==PictureToggle) {
         execute_kuip_cmd("Title");
      } else {
         if (CurrentToggle==XAxisToggle)
            histogram.XAxisTitle=NULL;
         if (CurrentToggle==YAxisToggle)
            histogram.YAxisTitle=NULL;
      }
   }
   if (InverseAuto) appres.auto_refresh = !appres.auto_refresh;
   if (appres.auto_refresh) plot_current_histo();
}

/*****************************************************************************
 *                                                                           *
 *       Manage Apply Button                                                 *
 *                                                                           *
 *****************************************************************************/
static  void ApplyAttributesDialog_cb (Widget w,caddr_t client_data,
                                       XmAnyCallbackStruct *call_data)
{
   plot_current_histo();
}

/*****************************************************************************
 *                                                                           *
 *       Manage RESET  Button                                                *
 *                                                                           *
 *****************************************************************************/
static  void ResetAttributesDialog_cb (Widget w,caddr_t client_data,
                                       XmAnyCallbackStruct *call_data)
{
   XmAnyCallbackStruct cb;
   int InverseAuto;


   /* Reset all Object by pushing the toggle of the button and calling
      the Reset for current object */

   cb.reason=0;
   cb.event=(XEvent *)NULL;
   if (appres.auto_refresh) {
      InverseAuto         = True;
      appres.auto_refresh = !appres.auto_refresh;
   } else {
      InverseAuto = False;
   }

   XmToggleButtonSetState(PictureToggle,True,True);
   XtCallCallbacks(ResetObjectBut,XmNactivateCallback,&cb);

   XmToggleButtonSetState(FunctionToggle,True,True);
   XtCallCallbacks(ResetObjectBut,XmNactivateCallback,&cb);

   XmToggleButtonSetState(XAxisToggle,True,True);
   XtCallCallbacks(ResetObjectBut,XmNactivateCallback,&cb);

   XmToggleButtonSetState(YAxisToggle,True,True);
   XtCallCallbacks(ResetObjectBut,XmNactivateCallback,&cb);

   XmToggleButtonSetState(HistoToggle,True,True);
   XtCallCallbacks(ResetObjectBut,XmNactivateCallback,&cb);

   XmToggleButtonSetState(BoxToggle,True,True);
   XtCallCallbacks(ResetObjectBut,XmNactivateCallback,&cb);

   if (InverseAuto) appres.auto_refresh = !appres.auto_refresh;
   /* -999 because of Global reset from histo.c don't want to replot */
   if ( (appres.auto_refresh) && (call_data->reason != -999)) {
      plot_current_histo();
   }
}

/*****************************************************************************
 *                                                                           *
 *   Manage ObjectRow                                                        *
 *                                                                           *
 *****************************************************************************/
static  void ObjectChange_cb(Widget w,caddr_t client_data,
                             XmAnyCallbackStruct *call_data)
{
   Widget CurrentObject;
   TStateObject *data;
   XmString Label;
   char *LabelBis;
   char *LabelTer;
   Boolean ret;
   XmAnyCallbackStruct CbText;
   XmToggleButtonCallbackStruct CbToggle;
   char Svalue[5];

   /* This function manage the change in the current object */

   XmRowColumnCallbackStruct *cb=( XmRowColumnCallbackStruct *) call_data;
   if (((XmToggleButtonCallbackStruct *)(cb->callbackstruct))->set) {

      /* only for activation */
      CurrentObject=cb->widget;
      XtVaGetValues(CurrentObject,XmNuserData,&data,NULL);

      /***** Line Width *****/

      /* value of line Width */
      sprintf(Svalue,"%d", data->LineWidth);
      XtVaSetValues(LineWidthValueLabel,XmNlabelString,
                    XmStringCreateLtoR(Svalue,XmSTRING_DEFAULT_CHARSET),NULL);

      /* Settings Label of Line Width */
      XtVaGetValues(cb->widget,XmNlabelString,&Label,NULL);
      XtVaSetValues(LineWidthLabel,XmNlabelString,XmStringConcat(Label,
                    XmStringCreateLtoR(" Line Width",XmSTRING_DEFAULT_CHARSET)),
                    NULL);
      /****** Title *****/
      /* Settings Label of Title Text */
      XtVaSetValues(TitleLabel,XmNlabelString,XmStringConcat(Label,
                   XmStringCreateLtoR(" Title",XmSTRING_DEFAULT_CHARSET)),NULL);
      /* Set value of text */
      XmTextSetString(TitleText,data->Title);

      /* No title for Box and Function */
      if ((CurrentObject==BoxToggle) || (CurrentObject==FunctionToggle))
         XtSetSensitive(TitleForm,False);
      else
         XtSetSensitive(TitleForm,True);

      /***** Hatch Style *******/
      if (widget_hatchStyle && XtIsManaged(widget_hatchStyle)) {

         /** Update title of Dialog **/
         LabelTer=XtCalloc(25,1);
         ret=XmStringGetLtoR(Label,XmSTRING_DEFAULT_CHARSET,&LabelBis);
         if (!ret)
            fprintf(stderr,"Unable to extract string in ObjectChange_cb\n");
         strcpy(LabelTer,LabelBis);
         XtVaSetValues(XtParent(widget_hatchStyle),XmNtitle,
                       strcat(LabelTer," Hatch Style")
                       ,NULL);
         XtFree(LabelBis);
         XtFree(LabelTer);
         /*** Update Value of hatch style ***/
         if ((CurrentObject==YAxisToggle)|| (CurrentObject==XAxisToggle))
            XtSetSensitive(widget_hatchStyle,False);
         else {
            XtSetSensitive(widget_hatchStyle,True);
            sprintf(Svalue,"%d", data->IndexHatch);
            AddZero(Svalue);
            XmTextSetString(IndexText,Svalue);
            CbText.reason=0;
            CbText.event=(XEvent*)NULL;
            XtCallCallbacks(IndexText,XmNactivateCallback,&CbText);
         } /* else */
      } /*  if (widget_hatchStyle) */
      else {
         /* if widget don't exist set unsensitive the button */
         if ((CurrentObject==YAxisToggle)|| (CurrentObject==XAxisToggle))
            XtSetSensitive(HatchStyleButton,False);
         else
            XtSetSensitive(HatchStyleButton,True);
      } /* else */

      /******** Color ********/

      if (widget_color && XtIsManaged(widget_color)) {

         /*** Dialog title ****/
         LabelTer=XtCalloc(25,1);
         ret=XmStringGetLtoR(Label,XmSTRING_DEFAULT_CHARSET,&LabelBis);
         if (!ret)
            fprintf(stderr,"Unable to extract string in ObjectChange_cb\n");
         strcpy(LabelTer,LabelBis);
         XtVaSetValues(XtParent(widget_color),XmNtitle,strcat(LabelTer," Color")
                       ,NULL);
         XtFree(LabelTer);
         XtFree(LabelBis);

         /**** Value ****/
         XtVaSetValues(ObjectRow,XmNmenuHistory,CurrentObject,NULL);
         /* VERY useful for NewTarget_cb because if not this fonction don't
            get the current object proprerly */
         CbToggle.event=(XEvent *)NULL;
         CbToggle.set=True;
         CbToggle.reason=0;
         if (data->SurfaceColor) {
            XmToggleButtonSetState(SurfaceToggle,True,True);
            XtCallCallbacks(SurfaceToggle,XmNarmCallback,&CbToggle);
         }
         else
         {
            XmToggleButtonSetState(ContourToggle,True,True);
            XtCallCallbacks(ContourToggle,XmNarmCallback,&CbToggle);

         }
         if (CurrentObject==BoxToggle) {
            XtSetSensitive(SurfaceToggle,True);
            XtSetSensitive(ZoneToggle,True);
            XtSetSensitive(StatToggle,True);
         } else {
            XtSetSensitive(ZoneToggle,False);
            XtSetSensitive(StatToggle,False);
            if ((CurrentObject==YAxisToggle) || (CurrentObject==XAxisToggle)) {
               XtSetSensitive(SurfaceToggle,False);
            } else
               XtSetSensitive(SurfaceToggle,True);
         }
      }
   }
}

/*****************************************************************************
 *                                                                           *
 *   Manage the Line Width scale                                             *
 *                                                                           *
 *****************************************************************************/
static  void LineWidthScale_cb(Widget w,caddr_t client_data,
                               XmAnyCallbackStruct *call_data)
{
   int LineWidth;
   Widget CurrentToggle;
   TStateObject *CurrentState;
   char *ComStr;
   char Svalue[6];

   /*   XmScaleGetValue(w,&LineWidth);*/
   XtVaGetValues(ObjectRow,XmNmenuHistory,&CurrentToggle,NULL);
   XtVaGetValues(CurrentToggle,XmNuserData,&CurrentState,NULL);
   LineWidth=CurrentState->LineWidth;
   switch((int) client_data) {
      case 1: /* UpArrow */
         LineWidth++;
         break;
      case 2: /* DownArrow*/
         LineWidth--;
         break;
      default :
         fprintf(stderr,"Internal error in function NewColor_cb");
   }
   if (LineWidth<0) LineWidth=0;

   if (CurrentState->LineWidth !=LineWidth) {
      Change=True;
      CurrentState->LineWidth=LineWidth;
   } else
      Change=False;

   if (Change) {
      ComStr=XtCalloc(MAX_CMD_LENGTH,1);
      sprintf(ComStr,"SET %sWID  %d",CurrentState->FirstLetter,LineWidth);
      execute_kuip_cmd(ComStr);
      if (appres.auto_refresh) {
         plot_current_histo();
      }
      XtFree(ComStr);

      sprintf(Svalue,"%d", LineWidth);
      XtVaSetValues(LineWidthValueLabel,XmNlabelString,
                    XmStringCreateLtoR(Svalue,XmSTRING_DEFAULT_CHARSET),NULL);
   }
}

/*****************************************************************************
 *                                                                           *
 *   Initialize the array StateObject                                        *
 *                                                                           *
 *****************************************************************************/
static void InitArrStateObject()
{
   int i=0;
   char temp[6];
   float currentValue;
   /* !!!!!!!!!!!!!! Never change order of object in array !!!!!!!! */

   StateObject[0].FirstLetter="B";
   StateObject[1].FirstLetter="H";
   StateObject[2].FirstLetter="Y";
   StateObject[3].FirstLetter="X";
   StateObject[4].FirstLetter="F";
   StateObject[5].FirstLetter="P";

   for(i=0;i<NBOBJECT;i++) {

      /* lineWidth */
      sprintf(temp,"?%sWID",StateObject[i].FirstLetter);
      Hplset(temp,&currentValue);
      StateObject[i].LineWidth=currentValue;

      /* Index Hatch */
      if ( (*StateObject[i].FirstLetter!='X') &&
           (*StateObject[i].FirstLetter!='Y') ) {
         sprintf(temp,"?%sTYP",StateObject[i].FirstLetter);
         Hplset(temp,&currentValue);
         StateObject[i].IndexHatch=currentValue;
      }

      /* Color */
      sprintf(temp,"?%sCOL",StateObject[i].FirstLetter);
      Hplset(temp,&currentValue);
      if (currentValue>=1000) {
         StateObject[i].ContourColor=0;
         StateObject[i].SurfaceColor=currentValue-1000;
      } else {
         StateObject[i].ContourColor=currentValue;
         StateObject[i].SurfaceColor=0;
      }
      if ((int) currentValue != currentValue) {
         /* stat and box shadow color != 0 */
         currentValue=currentValue-(int) currentValue;
         currentValue=currentValue*10;
         StateObject[i].ZoneColor=(int) currentValue;
         currentValue=currentValue-(int) currentValue;
         currentValue=currentValue*10;
         StateObject[i].StatColor=(int) currentValue;
      }

      /********* Problem for title :

         I have'nt got any function to get the current value of
         the title

         *********************************************/
      StateObject[i].Title[0]='\0';
      /*********** ^^^^^^^^^^^^^^^^^^^^^^^ To Change ************/
   }
   XtVaSetValues(BoxToggle,XmNuserData,&StateObject[0],NULL);
   XtVaSetValues(HistoToggle,XmNuserData,&StateObject[1],NULL);
   XtVaSetValues(YAxisToggle,XmNuserData,&StateObject[2],NULL);
   XtVaSetValues(XAxisToggle,XmNuserData,&StateObject[3],NULL);
   XtVaSetValues(FunctionToggle,XmNuserData,&StateObject[4],NULL);
   XtVaSetValues(PictureToggle,XmNuserData,&StateObject[5],NULL);
}

/***********************************************************************
 *                                                                     *
 *   Popdown callback. This callback is called when the AttributesForm *
 *   widget is popped down.                                            *
 *                                                                     *
 ***********************************************************************/
static void popdown_cb(Widget w, XtPointer cd, XmAnyCallbackStruct *cbs)
{
   /*
   *   Make sure popup's are unmanaged
   */
   if (widget_color) {
      XtUnmanageChild(widget_color);
      XtSetSensitive(ColorButton, True);
   }
   if (widget_hatchStyle) {
      XtUnmanageChild(widget_hatchStyle);
      XtSetSensitive(HatchStyleButton, True);
   }
}

/***********************************************************************
 *                                                                     *
 *   Unmanage dialog. This routine should be called by the             *
 *   popdown_cb routine of the parent.                                 *
 *                                                                     *
 ***********************************************************************/
void unmanage_attributes_dialog(void)
{
   if (widget_attributes) {
      MenuCbStruct  *item;
      XtVaGetValues(widget_attributes, XmNuserData, &item, NULL);
      popdown_argument_dialog(item);
   }
}

/*****************************************************************************
 *                                                                           *
 *   Initialize the attributes box and popup it                              *
 *                                                                           *
 *****************************************************************************/
void show_attributes(Widget w)
{
   MenuCbStruct *menu_button;


   if (!widget_attributes) {
      widget_attributes=create_attributesFormDialog();
      menu_button = make_menuStruct(w, widget_attributes);
      InstallMwmCloseCallback(XtParent(widget_attributes),
                              (XtCallbackProc)cancel_cb,
                              (XtPointer)menu_button);
      XtVaSetValues(widget_attributes, XmNuserData, menu_button, NULL);

      XtAddCallback(XtParent(widget_attributes), XmNpopdownCallback,
                    (XtCallbackProc)popdown_cb, NULL);
      XtAddCallback(HatchStyleButton,XmNactivateCallback,
                    (XtCallbackProc) show_hatchStyle,NULL);
      XtAddCallback(ColorButton,XmNactivateCallback,
                    (XtCallbackProc) show_color,NULL);

      XtAddCallback(TitleText,XmNactivateCallback,
                    (XtCallbackProc) TitleText_cb,(int *) 1);
      XtAddCallback(TitleText,XmNlosingFocusCallback,
                    (XtCallbackProc) TitleText_cb,(int *) 1);

      XtAddCallback(LineWidthUpArrow,XmNarmCallback,
                    (XtCallbackProc) LineWidthScale_cb,(int *) 1);
      XtAddCallback(LineWidthDownArrow,XmNarmCallback,
                    (XtCallbackProc) LineWidthScale_cb,(int *)2);

      XtAddCallback(ObjectRow,XmNentryCallback,
                    (XtCallbackProc) ObjectChange_cb,NULL);
      XtAddCallback(CloseAttributesBut,XmNactivateCallback,
                    (XtCallbackProc) cancel_cb,menu_button);
      XtAddCallback(ApplyAttributesBut,XmNactivateCallback,
                    (XtCallbackProc) ApplyAttributesDialog_cb,NULL);
      XtAddCallback(ResetAttributesBut,XmNactivateCallback,
                    (XtCallbackProc) ResetAttributesDialog_cb,NULL);
      XtAddCallback(ResetObjectBut,XmNactivateCallback,
                    (XtCallbackProc) ResetObject_cb,NULL);

      if (appres.auto_refresh) XtSetSensitive(ApplyAttributesBut,False);
      else XtSetSensitive(ApplyAttributesBut,True);

      InitArrStateObject();
      XmToggleButtonSetState(BoxToggle,True,True);
      XtVaSetValues(ObjectRow,XmNmenuHistory,BoxToggle,NULL);
      /* To be sure that history is <> form zero */
   }

   XtManageChild(widget_attributes);
   XtSetSensitive(w,False);
}

