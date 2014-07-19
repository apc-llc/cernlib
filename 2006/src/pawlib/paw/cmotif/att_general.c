/*
 * $Id: att_general.c,v 1.1.1.1 1996/03/01 11:38:53 mclareni Exp $
 *
 * $Log: att_general.c,v $
 * Revision 1.1.1.1  1996/03/01 11:38:53  mclareni
 * Paw
 *
 */
#include "paw/pilot.h"
/*CMZ :  2.05/01 22/03/94  15.04.18  by  Fons Rademakers*/
/*-- Author :*/
/*****************************************************************************
 *                                                                           *
 *  Att_general.c manage the widget_generalAttributes                        *
 *                                                                           *
 *****************************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <Xm/Xm.h>
#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <Xm/Scale.h>

#include "hmotif/uxxt.h"
#include "hmotif/pawm.h"
#include "hmotif/attstate.h"

#define line4_width 31
#define line4_height 11
static char line4_bits[] = {
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x4f, 0x9e, 0x3c, 0x79,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00};
#define line3_width 31
#define line3_height 11
static char line3_bits[] = {
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x49, 0x92, 0x24, 0x49,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00};

#define line2_width 31
#define line2_height 11
static char line2_bits[] = {
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xcf, 0xf3, 0x3c, 0x4f,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00};

#define line1_width 31
#define line1_height 11
static char line1_bits[] = {
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xff, 0xff, 0xff, 0x7f,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00};

#define mk1_width 20
#define mk1_height 20
static char mk1_bits[] = {
   0x00, 0x00, 0xf0, 0x00, 0x00, 0xf0, 0x00, 0x00, 0xf0, 0x00, 0x00, 0xf0,
   0x00, 0x00, 0xf0, 0x00, 0x00, 0xf0, 0x00, 0x00, 0xf0, 0x00, 0x00, 0xf0,
   0x00, 0x00, 0xf0, 0x00, 0x06, 0xf0, 0x00, 0x06, 0xf0, 0x00, 0x00, 0xf0,
   0x00, 0x00, 0xf0, 0x00, 0x00, 0xf0, 0x00, 0x00, 0xf0, 0x00, 0x00, 0xf0,
   0x00, 0x00, 0xf0, 0x00, 0x00, 0xf0, 0x00, 0x00, 0xf0, 0x00, 0x00, 0xf0};

#define mk2_width 20
#define mk2_height 20
static char mk2_bits[] = {
   0x00, 0x02, 0xf0, 0x00, 0x02, 0xf0, 0x00, 0x02, 0xf0, 0x00, 0x02, 0xf0,
   0x00, 0x02, 0xf0, 0x00, 0x02, 0xf0, 0x00, 0x02, 0xf0, 0x00, 0x02, 0xf0,
   0x00, 0x02, 0xf0, 0xff, 0xff, 0xff, 0x00, 0x02, 0xf0, 0x00, 0x02, 0xf0,
   0x00, 0x02, 0xf0, 0x00, 0x02, 0xf0, 0x00, 0x02, 0xf0, 0x00, 0x02, 0xf0,
   0x00, 0x02, 0xf0, 0x00, 0x02, 0xf0, 0x00, 0x02, 0xf0, 0x00, 0x02, 0xf0};

#define mk3_width 20
#define mk3_height 20
static char mk3_bits[] = {
   0x01, 0x02, 0xf4, 0x02, 0x02, 0xf2, 0x04, 0x02, 0xf1, 0x08, 0x82, 0xf0,
   0x10, 0x42, 0xf0, 0x20, 0x22, 0xf0, 0x40, 0x12, 0xf0, 0x80, 0x0a, 0xf0,
   0x00, 0x07, 0xf0, 0xff, 0xff, 0xff, 0x00, 0x07, 0xf0, 0x80, 0x0a, 0xf0,
   0x40, 0x12, 0xf0, 0x20, 0x22, 0xf0, 0x10, 0x42, 0xf0, 0x08, 0x82, 0xf0,
   0x04, 0x02, 0xf1, 0x02, 0x02, 0xf2, 0x01, 0x02, 0xf4, 0x00, 0x02, 0xf8};

#define mk4_width 20
#define mk4_height 20
static char mk4_bits[] = {
   0x00, 0x00, 0xf0, 0x80, 0x3f, 0xf0, 0x60, 0xc0, 0xf0, 0x10, 0x00, 0xf1,
   0x08, 0x00, 0xf2, 0x04, 0x00, 0xf4, 0x04, 0x00, 0xf4, 0x02, 0x00, 0xf8,
   0x02, 0x00, 0xf8, 0x02, 0x00, 0xf8, 0x02, 0x00, 0xf8, 0x02, 0x00, 0xf8,
   0x02, 0x00, 0xf8, 0x02, 0x00, 0xf8, 0x04, 0x00, 0xf4, 0x04, 0x00, 0xf4,
   0x08, 0x00, 0xf2, 0x10, 0x00, 0xf1, 0x60, 0xc0, 0xf0, 0x80, 0x3f, 0xf0};

#define mk5_width 20
#define mk5_height 20
static char mk5_bits[] = {
   0x01, 0x00, 0xf4, 0x02, 0x00, 0xf2, 0x04, 0x00, 0xf1, 0x08, 0x80, 0xf0,
   0x10, 0x40, 0xf0, 0x20, 0x20, 0xf0, 0x40, 0x10, 0xf0, 0x80, 0x08, 0xf0,
   0x00, 0x05, 0xf0, 0x00, 0x02, 0xf0, 0x00, 0x05, 0xf0, 0x80, 0x08, 0xf0,
   0x40, 0x10, 0xf0, 0x20, 0x20, 0xf0, 0x10, 0x40, 0xf0, 0x08, 0x80, 0xf0,
   0x04, 0x00, 0xf1, 0x02, 0x00, 0xf2, 0x01, 0x00, 0xf4, 0x00, 0x00, 0xf8};

#define mk23_width 20
#define mk23_height 20
static char mk23_bits[] = {
   0x00, 0x00, 0xf0, 0xff, 0xff, 0xf7, 0x02, 0x00, 0xf2, 0x92, 0x48, 0xf2,
   0x24, 0x25, 0xf1, 0x24, 0x25, 0xf1, 0x48, 0x92, 0xf0, 0x48, 0x90, 0xf0,
   0x90, 0x48, 0xf0, 0x90, 0x28, 0xf0, 0x20, 0x25, 0xf0, 0x40, 0x15, 0xf0,
   0x40, 0x12, 0xf0, 0x80, 0x08, 0xf0, 0x80, 0x08, 0xf0, 0x00, 0x05, 0xf0,
   0x00, 0x05, 0xf0, 0x00, 0x02, 0xf0, 0x00, 0x00, 0xf0, 0x00, 0x00, 0xf0};

#define mk24_width 20
#define mk24_height 20
static char mk24_bits[] = {
   0x00, 0x00, 0xf0, 0x80, 0x0f, 0xf0, 0x60, 0x30, 0xf0, 0x10, 0x40, 0xf0,
   0x08, 0x80, 0xf0, 0x04, 0x00, 0xf1, 0x04, 0x00, 0xf1, 0x02, 0x00, 0xf2,
   0x02, 0x00, 0xf2, 0x02, 0x00, 0xf2, 0x02, 0x00, 0xf2, 0x02, 0x00, 0xf2,
   0x04, 0x00, 0xf1, 0x04, 0x00, 0xf1, 0x08, 0x80, 0xf0, 0x10, 0x40, 0xf0,
   0x60, 0x30, 0xf0, 0x80, 0x0f, 0xf0, 0x00, 0x00, 0xf0, 0x00, 0x00, 0xf0};

#define mk22_width 20
#define mk22_height 20
static char mk22_bits[] = {
   0x00, 0x00, 0xf0, 0x00, 0x02, 0xf0, 0x00, 0x05, 0xf0, 0x00, 0x05, 0xf0,
   0x80, 0x08, 0xf0, 0x80, 0x08, 0xf0, 0x40, 0x12, 0xf0, 0x40, 0x15, 0xf0,
   0x20, 0x25, 0xf0, 0x90, 0x28, 0xf0, 0x90, 0x48, 0xf0, 0x48, 0x90, 0xf0,
   0x48, 0x92, 0xf0, 0x24, 0x25, 0xf1, 0x24, 0x25, 0xf1, 0x92, 0x48, 0xf2,
   0x02, 0x00, 0xf2, 0xff, 0xff, 0xf7, 0x00, 0x00, 0xf0, 0x00, 0x00, 0xf0};

#define mk25_width 20
#define mk25_height 20
static char mk25_bits[] = {
   0x00, 0x00, 0xf0, 0xfe, 0xff, 0xf3, 0x02, 0x00, 0xf2, 0x02, 0x00, 0xf2,
   0x02, 0x00, 0xf2, 0x02, 0x00, 0xf2, 0x02, 0x00, 0xf2, 0x02, 0x00, 0xf2,
   0x02, 0x00, 0xf2, 0x02, 0x00, 0xf2, 0x02, 0x00, 0xf2, 0x02, 0x00, 0xf2,
   0x02, 0x00, 0xf2, 0x02, 0x00, 0xf2, 0x02, 0x00, 0xf2, 0x02, 0x00, 0xf2,
   0x02, 0x00, 0xf2, 0xfe, 0xff, 0xf3, 0x00, 0x00, 0xf0, 0x00, 0x00, 0xf0};

#define mk30_width 20
#define mk30_height 20
static char mk30_bits[] = {
   0x00, 0x00, 0xf0, 0x00, 0x02, 0xf0, 0x00, 0x02, 0xf0, 0x00, 0x05, 0xf0,
   0x00, 0x05, 0xf0, 0x00, 0x05, 0xf0, 0x80, 0x08, 0xf0, 0xfe, 0xf8, 0xf3,
   0x04, 0x00, 0xf1, 0x18, 0xc0, 0xf0, 0x20, 0x20, 0xf0, 0x40, 0x10, 0xf0,
   0x40, 0x10, 0xf0, 0x20, 0x22, 0xf0, 0x20, 0x25, 0xf0, 0xe0, 0x38, 0xf0,
   0x30, 0x60, 0xf0, 0x10, 0x40, 0xf0, 0x00, 0x00, 0xf0, 0x00, 0x00, 0xf0};

#define mk21_width 20
#define mk21_height 20
static char mk21_bits[] = {
   0x00, 0x00, 0xf0, 0xfe, 0xff, 0xf3, 0x02, 0x00, 0xf2, 0xaa, 0xaa, 0xf2,
   0xaa, 0xaa, 0xf2, 0xaa, 0xaa, 0xf2, 0xaa, 0xaa, 0xf2, 0xaa, 0xaa, 0xf2,
   0xaa, 0xaa, 0xf2, 0xaa, 0xaa, 0xf2, 0xaa, 0xaa, 0xf2, 0xaa, 0xaa, 0xf2,
   0xaa, 0xaa, 0xf2, 0xaa, 0xaa, 0xf2, 0xaa, 0xaa, 0xf2, 0xaa, 0xaa, 0xf2,
   0x02, 0x00, 0xf2, 0xfe, 0xff, 0xf3, 0x00, 0x00, 0xf0, 0x00, 0x00, 0xf0};

#define mk26_width 20
#define mk26_height 20
static char mk26_bits[] = {
   0x00, 0x00, 0xf0, 0x00, 0x02, 0xf0, 0x00, 0x05, 0xf0, 0x00, 0x05, 0xf0,
   0x80, 0x08, 0xf0, 0x80, 0x08, 0xf0, 0x40, 0x10, 0xf0, 0x40, 0x10, 0xf0,
   0x20, 0x20, 0xf0, 0x10, 0x20, 0xf0, 0x10, 0x40, 0xf0, 0x08, 0x80, 0xf0,
   0x08, 0x80, 0xf0, 0x04, 0x00, 0xf1, 0x04, 0x00, 0xf1, 0x02, 0x00, 0xf2,
   0x02, 0x00, 0xf2, 0xff, 0xff, 0xf7, 0x00, 0x00, 0xf0, 0x00, 0x00, 0xf0};

#define mk27_width 20
#define mk27_height 20
static char mk27_bits[] = {
   0x00, 0x00, 0xf0, 0x00, 0x02, 0xf0, 0x00, 0x05, 0xf0, 0x00, 0x05, 0xf0,
   0x80, 0x08, 0xf0, 0x40, 0x08, 0xf0, 0x40, 0x10, 0xf0, 0x20, 0x20, 0xf0,
   0x20, 0x20, 0xf0, 0x10, 0x40, 0xf0, 0x20, 0x20, 0xf0, 0x20, 0x20, 0xf0,
   0x40, 0x10, 0xf0, 0x40, 0x08, 0xf0, 0x80, 0x08, 0xf0, 0x00, 0x05, 0xf0,
   0x00, 0x05, 0xf0, 0x00, 0x02, 0xf0, 0x00, 0x00, 0xf0, 0x00, 0x00, 0xf0};

#define mk28_width 20
#define mk28_height 20
static char mk28_bits[] = {
   0x00, 0x00, 0xf0, 0xc0, 0x1f, 0xf0, 0x40, 0x10, 0xf0, 0x40, 0x10, 0xf0,
   0x40, 0x10, 0xf0, 0x40, 0x10, 0xf0, 0x7e, 0xf0, 0xf3, 0x02, 0x00, 0xf2,
   0x02, 0x00, 0xf2, 0x02, 0x00, 0xf2, 0x02, 0x00, 0xf2, 0x02, 0x00, 0xf2,
   0x7e, 0xf0, 0xf3, 0x40, 0x10, 0xf0, 0x40, 0x10, 0xf0, 0x40, 0x10, 0xf0,
   0x40, 0x10, 0xf0, 0xc0, 0x1f, 0xf0, 0x00, 0x00, 0xf0, 0x00, 0x00, 0xf0};

#define mk29_width 20
#define mk29_height 20
static char mk29_bits[] = {
   0x00, 0x00, 0xf0, 0x00, 0x02, 0xf0, 0x00, 0x02, 0xf0, 0x00, 0x07, 0xf0,
   0x00, 0x07, 0xf0, 0x80, 0x07, 0xf0, 0x80, 0x0f, 0xf0, 0xfe, 0xff, 0xf3,
   0xfc, 0xff, 0xf1, 0xf0, 0x7f, 0xf0, 0xe0, 0x3f, 0xf0, 0xe0, 0x3f, 0xf0,
   0xe0, 0x3f, 0xf0, 0xf0, 0x3f, 0xf0, 0xf0, 0x78, 0xf0, 0x70, 0x70, 0xf0,
   0x38, 0xe0, 0xf0, 0x08, 0x80, 0xf0, 0x00, 0x00, 0xf0, 0x00, 0x00, 0xf0};

#define mk31_width 20
#define mk31_height 20
static char mk31_bits[] = {
   0x00, 0x00, 0xf0, 0x00, 0x02, 0xf0, 0x00, 0x02, 0xf0, 0x00, 0x02, 0xf0,
   0x02, 0x02, 0xf2, 0x0c, 0x82, 0xf1, 0x30, 0x62, 0xf0, 0x40, 0x12, 0xf0,
   0x80, 0x0f, 0xf0, 0x00, 0x02, 0xf0, 0x80, 0x0f, 0xf0, 0x60, 0x32, 0xf0,
   0x10, 0x42, 0xf0, 0x0c, 0x82, 0xf1, 0x02, 0x02, 0xf2, 0x00, 0x02, 0xf0,
   0x00, 0x02, 0xf0, 0x00, 0x02, 0xf0, 0x00, 0x00, 0xf0, 0x00, 0x00, 0xf0};

#define mk20_width 20
#define mk20_height 20
static char mk20_bits[] = {
   0x00, 0x00, 0xf0, 0xc0, 0x1f, 0xf0, 0x30, 0x60, 0xf0, 0x88, 0x8a, 0xf0,
   0x84, 0x8a, 0xf0, 0xa4, 0x2a, 0xf1, 0xa2, 0x2a, 0xf1, 0xaa, 0xaa, 0xf2,
   0xaa, 0xaa, 0xf2, 0xaa, 0xaa, 0xf2, 0xaa, 0xaa, 0xf2, 0xaa, 0xaa, 0xf2,
   0xa2, 0x2a, 0xf1, 0xa4, 0x2a, 0xf1, 0x84, 0x8a, 0xf0, 0x98, 0xca, 0xf0,
   0x60, 0x30, 0xf0, 0x80, 0x0f, 0xf0, 0x00, 0x00, 0xf0, 0x00, 0x00, 0xf0};

/* extern functions */
extern void show_defineColor (Widget w,caddr_t client_data,
                              XmAnyCallbackStruct *call_data);
extern Widget create_generalAttributesDialog();

/* extern widgets */
extern Widget MarkerMenu,LineMenu;
extern Widget Line1,Line2,Line3,Line4;
extern Widget ApplyGeneralBut,ResetGeneralBut,CloseGeneralBut;
extern Widget Marker1,Marker2,Marker3,Marker4,Marker5,Marker20,Marker21,
              Marker22,Marker23,Marker24,Marker25,Marker26,Marker27,
              Marker28,Marker29,Marker30,Marker31;

extern Widget GeneralButton;
extern Widget DefineColorButton,ResetColorButton;
extern Widget MarkerSizeScale;
extern Widget widget_defineColor;

/* global data */
Widget widget_generalAttributes = (Widget) NULL;
float MSCF;

/* global function */

/* local function */
static  void LineMenu_cb(Widget w,caddr_t client_data,
                         XmAnyCallbackStruct *call_data);
static  void MarkerMenu_cb(Widget w,caddr_t client_data,
                           XmAnyCallbackStruct *call_data);

static  void ResetGeneralDialog_cb (Widget w,caddr_t client_data,
                                    XmAnyCallbackStruct *call_data);
static  void ApplyGeneralDialog_cb (Widget w,caddr_t client_data,
                                    XmAnyCallbackStruct *call_data);

/* local data */
static int Change;

/*****************************************************************************
 *                                                                           *
 *     put attributes of widget_generalAttributes                            *
 *                                                                           *
 *****************************************************************************/
void put_attributes_generalAttribute(char *label,char *value)
{
   int tempint;
   double tempfloat;

   if (!strcmp(label,"MTYP")) {
      tempint=atoi(value);
      switch(tempint) {
         case 1:
            XtVaSetValues(MarkerMenu,XmNmenuHistory,Marker1,NULL);
            break;
         case 2 :
            XtVaSetValues(MarkerMenu,XmNmenuHistory,Marker2,NULL);
            break;
         case 3 :
            XtVaSetValues(MarkerMenu,XmNmenuHistory,Marker3,NULL);
            break;
         case 4:
            XtVaSetValues(MarkerMenu,XmNmenuHistory,Marker4,NULL);
            break;
         case 5 :
            XtVaSetValues(MarkerMenu,XmNmenuHistory,Marker5,NULL);
            break;
         case 20 :
            XtVaSetValues(MarkerMenu,XmNmenuHistory,Marker20,NULL);
            break;
         case 21:
            XtVaSetValues(MarkerMenu,XmNmenuHistory,Marker21,NULL);
            break;
         case 22 :
            XtVaSetValues(MarkerMenu,XmNmenuHistory,Marker22,NULL);
            break;
         case 23 :
            XtVaSetValues(MarkerMenu,XmNmenuHistory,Marker23,NULL);
            break;
         case 24:
            XtVaSetValues(MarkerMenu,XmNmenuHistory,Marker24,NULL);
            break;
         case 25 :
            XtVaSetValues(MarkerMenu,XmNmenuHistory,Marker25,NULL);
            break;
         case 26 :
           XtVaSetValues(MarkerMenu,XmNmenuHistory,Marker26,NULL);
            break;
         case 27:
            XtVaSetValues(MarkerMenu,XmNmenuHistory,Marker27,NULL);
            break;
         case 28 :
            XtVaSetValues(MarkerMenu,XmNmenuHistory,Marker28,NULL);
            break;
         case 29 :
           XtVaSetValues(MarkerMenu,XmNmenuHistory,Marker29,NULL);
            break;
         case 30:
            XtVaSetValues(MarkerMenu,XmNmenuHistory,Marker30,NULL);
            break;
         case 31 :
            XtVaSetValues(MarkerMenu,XmNmenuHistory,Marker31,NULL);
            break;
         default:
            fprintf(stderr,
                    "unknown Marker type in put_attributes_generalAttribute");
      }
   }
   if ((!strcmp(label,"LTYP")) || (!strcmp(label,"DMOD")) )  {
      tempint=atoi(value);
      switch(tempint) {
         case 1:
            XtVaSetValues(LineMenu,XmNmenuHistory,Line1,NULL);
            break;
         case 2:
            XtVaSetValues(LineMenu,XmNmenuHistory,Line2,NULL);
            break;
         case 3:
            XtVaSetValues(LineMenu,XmNmenuHistory,Line3,NULL);
            break;
         case 4:
            XtVaSetValues(LineMenu,XmNmenuHistory,Line4,NULL);
            break;
         default :
            fprintf(stderr,"Unknown line type \n");
      }
   }
   if (!strcmp(label,"MSCF")) {
      tempfloat=atof(value);
      tempint=tempfloat*10;
      XmScaleSetValue(MarkerSizeScale,tempint);
   }
}

/*****************************************************************************
 *                                                                           *
 *        give attributes of widget_generalAttributes                        *
 *                                                                           *
 *****************************************************************************/
char *get_attributes_generalAttribute(char *label)
{
   char *temp;
   float r;

   temp=XtCalloc(10,1);

   if(!strcmp(label, "DMOD")) {
      Hplset("?DMOD",&r);
      sprintf(temp,"%d",(int) r);
  }
  else {
       Igq(label,&r);
       sprintf(temp,"%d", (int)r);
 }
  return temp;
   /**** !!!!!!!!!!! temp must be free by calling function */
}

/*****************************************************************************
 *                                                                           *
 *   Manage The Marker Size Scale                                            *
 *                                                                           *
 *****************************************************************************/
static void MoveMarkerScale_cb(Widget w,caddr_t client_data,
                               XmAnyCallbackStruct *call_data)
{
   float size;
   int Ivalue;
   char *ComStr;

   XmScaleGetValue(MarkerSizeScale,&Ivalue);
   size=Ivalue;
   size=size/10;
   if (MSCF!=size) {
      MSCF=size;
      Change=True;
   } else
      Change=False;

   if (Change) {
      ComStr=XtCalloc(MAX_CMD_LENGTH,1);
      sprintf(ComStr,"IGSET MSCF %f",MSCF);
      execute_kuip_cmd(ComStr);
      if (appres.auto_refresh) {
         plot_current_histo();
      }
      XtFree(ComStr);
   }
}

/*****************************************************************************
 *                                                                           *
 *       Manage Apply Button                                                 *
 *                                                                           *
 *****************************************************************************/
static  void ApplyGeneralDialog_cb (Widget w,caddr_t client_data,
                                    XmAnyCallbackStruct *call_data)
{
   plot_current_histo();
}

/*****************************************************************************
 *                                                                           *
 *       Manage RESET  Button                                                *
 *                                                                           *
 *****************************************************************************/
static  void ResetGeneralDialog_cb (Widget w,caddr_t client_data,
                                    XmAnyCallbackStruct *call_data)
{
   XmAnyCallbackStruct cb;

   execute_kuip_cmd("IGSET MTYP 1");
   execute_kuip_cmd("IGSET LTYP 1");
   execute_kuip_cmd("IGSET MSCF 1");

   execute_kuip_cmd("SET DMOD 1");

   XtVaSetValues(MarkerMenu,XmNmenuHistory,Marker1,NULL);
   XtVaSetValues(LineMenu,XmNmenuHistory,Line1,NULL);
   XmScaleSetValue(MarkerSizeScale,10);
   if (widget_defineColor) {
      if (call_data->reason != -999)
        cb.reason=0;
      else
         cb.reason= -999;
      cb.event=(XEvent *) NULL;
      XtCallCallbacks(ResetColorButton,XmNactivateCallback,&cb);
   }

   /*   if (appres.auto_refresh) {*/
   /* -999 because of Global reset from histo.c don't want to replot */
      if ( (appres.auto_refresh) && (call_data->reason != -999)) {
      plot_current_histo();
   }
}

/*****************************************************************************
 *                                                                           *
 *   Manage the Marker Menu                                                  *
 *                                                                           *
 *****************************************************************************/
static  void MarkerMenu_cb(Widget w,caddr_t client_data,
                           XmAnyCallbackStruct *call_data)
{
   char ComStr[15];

   sprintf(ComStr,"IGSET MTYP %d",(int) client_data);
   execute_kuip_cmd(ComStr);
   if (appres.auto_refresh) {
      plot_current_histo();
   }
}

/*****************************************************************************
 *                                                                           *
 *    Manage the line menu option                                            *
 *                                                                           *
 *****************************************************************************/
static  void LineMenu_cb(Widget w,caddr_t client_data,
                         XmAnyCallbackStruct *call_data)
{
   char ComStr[15];

   sprintf(ComStr,"IGSET LTYP %d",(int) client_data);
   sprintf(ComStr,"SET DMOD %d",(int) client_data);

   execute_kuip_cmd(ComStr);
   if (appres.auto_refresh) {
      plot_current_histo();
   }
}

/*****************************************************************************
 *                                                                           *
 *   Popdown callback. This callback is called when the AttributesForm       *
 *   widget is popped down.                                                  *
 *                                                                           *
 *****************************************************************************/
static void popdown_cb(Widget w, XtPointer cd, XmAnyCallbackStruct *cbs)
{
    /*
     *   Make sure popup's are unmanaged
     */
    if (widget_defineColor) {
       XtUnmanageChild(widget_defineColor);
       XtSetSensitive(DefineColorButton, True);
    }
}

/***********************************************************************
 *                                                                     *
 *   Popdown callback. This callback is called when the                *
 *   columnSelection widget is popped down.                            *
 *                                                                     *
 ***********************************************************************/
void unmanage_generalAttributes(void)
{
   if (widget_generalAttributes) {
      MenuCbStruct  *item;
      XtVaGetValues(widget_generalAttributes, XmNuserData, &item, NULL);
      popdown_argument_dialog(item);
   }
}

/*****************************************************************************
 *                                                                           *
 *   Initialize the attributes box and popup it                              *
 *                                                                           *
 *****************************************************************************/
void show_generalAttributes(Widget w)
{
   MenuCbStruct *menu_button;
   float currentValue;
   int tempInt;

   if (!widget_generalAttributes) {
      widget_generalAttributes=create_generalAttributesDialog();
      menu_button = make_menuStruct(w, widget_generalAttributes);
      InstallMwmCloseCallback(XtParent(widget_generalAttributes),
                              (XtCallbackProc)cancel_cb,
                              (XtPointer)menu_button);
      /* store MenuCbSruct in userData */
      XtVaSetValues(widget_generalAttributes, XmNuserData, menu_button, NULL);

      XtAddCallback(XtParent(widget_generalAttributes), XmNpopdownCallback,
                    (XtCallbackProc)popdown_cb, NULL);
      XtAddCallback(DefineColorButton,XmNactivateCallback,
                    (XtCallbackProc) show_defineColor,NULL);
      XtAddCallback(MarkerSizeScale,XmNdragCallback,
                    (XtCallbackProc) MoveMarkerScale_cb,(int *)1);
      XtAddCallback(MarkerSizeScale,XmNvalueChangedCallback,
                    (XtCallbackProc) MoveMarkerScale_cb,(int *)1);

      XtAddCallback(Line1,XmNactivateCallback,
                   (XtCallbackProc) LineMenu_cb,(int *)1);
      XtAddCallback(Line2,XmNactivateCallback,
                   (XtCallbackProc) LineMenu_cb,(int *)2);
      XtAddCallback(Line3,XmNactivateCallback,
                   (XtCallbackProc) LineMenu_cb,(int *)3);
      XtAddCallback(Line4,XmNactivateCallback,
                   (XtCallbackProc) LineMenu_cb,(int *)4);

      XtAddCallback(Marker1,XmNactivateCallback,
                   (XtCallbackProc) MarkerMenu_cb,(int *)1);
      XtAddCallback(Marker2,XmNactivateCallback,
                   (XtCallbackProc) MarkerMenu_cb,(int *)2);
      XtAddCallback(Marker3,XmNactivateCallback,
                   (XtCallbackProc) MarkerMenu_cb,(int *)3);
      XtAddCallback(Marker4,XmNactivateCallback,
                   (XtCallbackProc) MarkerMenu_cb,(int *)4);
      XtAddCallback(Marker5,XmNactivateCallback,
                   (XtCallbackProc) MarkerMenu_cb,(int *)5);
      XtAddCallback(Marker20,XmNactivateCallback,
                   (XtCallbackProc) MarkerMenu_cb,(int *)20);
      XtAddCallback(Marker21,XmNactivateCallback,
                   (XtCallbackProc) MarkerMenu_cb,(int *)21);
      XtAddCallback(Marker22,XmNactivateCallback,
                   (XtCallbackProc) MarkerMenu_cb,(int *)22);
      XtAddCallback(Marker23,XmNactivateCallback,
                   (XtCallbackProc) MarkerMenu_cb,(int *)23);
      XtAddCallback(Marker24,XmNactivateCallback,
                   (XtCallbackProc) MarkerMenu_cb,(int *)24);
      XtAddCallback(Marker25,XmNactivateCallback,
                   (XtCallbackProc) MarkerMenu_cb,(int *)25);
      XtAddCallback(Marker26,XmNactivateCallback,
                   (XtCallbackProc) MarkerMenu_cb,(int *)26);
      XtAddCallback(Marker27,XmNactivateCallback,
                   (XtCallbackProc) MarkerMenu_cb,(int *)27);
      XtAddCallback(Marker28,XmNactivateCallback,
                   (XtCallbackProc) MarkerMenu_cb,(int *)28);
      XtAddCallback(Marker29,XmNactivateCallback,
                   (XtCallbackProc) MarkerMenu_cb,(int *)29);
      XtAddCallback(Marker30,XmNactivateCallback,
                   (XtCallbackProc) MarkerMenu_cb,(int *)30);
      XtAddCallback(Marker31,XmNactivateCallback,
                   (XtCallbackProc) MarkerMenu_cb,(int *)31);

      XtAddCallback(CloseGeneralBut,XmNactivateCallback,
                   (XtCallbackProc) cancel_cb,menu_button);
      XtAddCallback(ApplyGeneralBut,XmNactivateCallback,
                   (XtCallbackProc) ApplyGeneralDialog_cb,NULL);
      XtAddCallback(ResetGeneralBut,XmNactivateCallback,
                   (XtCallbackProc) ResetGeneralDialog_cb,NULL);

      set_pixmap_to_button(Line4, line4_bits, line4_width, line4_height,
                           True);
      set_pixmap_to_button(Line3, line3_bits, line3_width, line3_height,
                           True);
      set_pixmap_to_button(Line2, line2_bits, line2_width, line2_height,
                           True);
      set_pixmap_to_button(Line1, line1_bits, line1_width, line1_height,
                           True);
      set_pixmap_to_button(Marker1, mk1_bits, mk1_width, mk1_height,
                           True);
      set_pixmap_to_button(Marker2, mk2_bits, mk2_width, mk2_height,
                           True);
      set_pixmap_to_button(Marker3, mk3_bits, mk3_width, mk3_height,
                           True);
      set_pixmap_to_button(Marker4, mk4_bits, mk4_width, mk4_height,
                           True);
      set_pixmap_to_button(Marker5, mk5_bits, mk5_width, mk5_height,
                           True);
      set_pixmap_to_button(Marker20, mk20_bits, mk20_width, mk20_height,
                           True);
      set_pixmap_to_button(Marker21, mk21_bits, mk21_width, mk21_height,
                           True);
      set_pixmap_to_button(Marker22, mk22_bits, mk22_width, mk22_height,
                           True);
      set_pixmap_to_button(Marker23, mk23_bits, mk23_width, mk23_height,
                           True);
      set_pixmap_to_button(Marker24, mk24_bits, mk24_width, mk24_height,
                           True);
      set_pixmap_to_button(Marker25, mk25_bits, mk25_width, mk25_height,
                           True);
      set_pixmap_to_button(Marker26, mk26_bits, mk26_width, mk26_height,
                           True);
      set_pixmap_to_button(Marker27, mk27_bits, mk27_width, mk27_height,
                           True);
      set_pixmap_to_button(Marker28, mk28_bits, mk28_width, mk28_height,
                           True);
      set_pixmap_to_button(Marker29, mk29_bits, mk29_width, mk29_height,
                           True);
      set_pixmap_to_button(Marker30, mk30_bits, mk30_width, mk30_height,
                           True);
      set_pixmap_to_button(Marker31, mk31_bits, mk31_width, mk31_height,
                           True);

      if (appres.auto_refresh) XtSetSensitive(ApplyGeneralBut,False);
      else XtSetSensitive(ApplyGeneralBut,True);
      /* initialize value of MSCF */

      Igq("MSCF",&MSCF);
      tempInt=MSCF*10;
      XmScaleSetValue(MarkerSizeScale,tempInt);

      /* Initialize Ltyp */
      Hplset("?DMOD",&currentValue);
      switch ((int) currentValue) {
         case 1:
            XtVaSetValues(LineMenu,XmNmenuHistory,Line1,NULL);
            break;
         case 2:
            XtVaSetValues(LineMenu,XmNmenuHistory,Line2,NULL);
            break;
         case 3:
            XtVaSetValues(LineMenu,XmNmenuHistory,Line3,NULL);
            break;
         case 4:
            XtVaSetValues(LineMenu,XmNmenuHistory,Line4,NULL);
            break;
         default :
            fprintf(stderr,"Unknown line type \n");
      }
      /* init Mtyp */
      Igq("MTYP",&currentValue);
      switch ((int) currentValue) {
         case 1:
            XtVaSetValues(MarkerMenu,XmNmenuHistory,Marker1,NULL);
            break;
         case 2 :
            XtVaSetValues(MarkerMenu,XmNmenuHistory,Marker2,NULL);
            break;
         case 3 :
            XtVaSetValues(MarkerMenu,XmNmenuHistory,Marker3,NULL);
            break;
         case 4:
            XtVaSetValues(MarkerMenu,XmNmenuHistory,Marker4,NULL);
            break;
         case 5 :
            XtVaSetValues(MarkerMenu,XmNmenuHistory,Marker5,NULL);
            break;
         case 20 :
            XtVaSetValues(MarkerMenu,XmNmenuHistory,Marker20,NULL);
            break;
         case 21:
            XtVaSetValues(MarkerMenu,XmNmenuHistory,Marker21,NULL);
            break;
         case 22 :
            XtVaSetValues(MarkerMenu,XmNmenuHistory,Marker22,NULL);
            break;
         case 23 :
            XtVaSetValues(MarkerMenu,XmNmenuHistory,Marker23,NULL);
            break;
         case 24:
            XtVaSetValues(MarkerMenu,XmNmenuHistory,Marker24,NULL);
            break;
         case 25 :
            XtVaSetValues(MarkerMenu,XmNmenuHistory,Marker25,NULL);
            break;
         case 26 :
           XtVaSetValues(MarkerMenu,XmNmenuHistory,Marker26,NULL);
            break;
         case 27:
            XtVaSetValues(MarkerMenu,XmNmenuHistory,Marker27,NULL);
            break;
         case 28 :
            XtVaSetValues(MarkerMenu,XmNmenuHistory,Marker28,NULL);
            break;
         case 29 :
           XtVaSetValues(MarkerMenu,XmNmenuHistory,Marker29,NULL);
            break;
         case 30:
            XtVaSetValues(MarkerMenu,XmNmenuHistory,Marker30,NULL);
            break;
         case 31 :
            XtVaSetValues(MarkerMenu,XmNmenuHistory,Marker31,NULL);
            break;

         default:
            fprintf(stderr,"unknown Marker type" );
      }
   }
   XtManageChild(widget_generalAttributes);
   XtSetSensitive(w,False);
}

