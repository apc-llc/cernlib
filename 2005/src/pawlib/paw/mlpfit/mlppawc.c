#include <assert.h>
#include <ctype.h>
#include <errno.h>
#include <limits.h>
#include <math.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <cfortran/cfortran.h> 
#include <cfortran/hbook.h>
#include <cfortran/higz.h>
#include <cfortran/hplot.h>
#include <cfortran/kuip.h>

#include "mlp_inter.h"
#include "../ntuple/str.h"

extern void     ku_alfa( void );
extern char *   ku_getc( void );
extern char *   ku_getf( void );
extern int      ku_geti( void );
extern char *   ku_gets( void );
extern char *   ku_getl( void );
extern int      ku_npar( void );
extern char *   ku_path( void );
extern double   ku_getr( void );
extern char *   ku_proc( const char* prompt, const char* dfault );
extern int      ku_exec( const char* cmd );

/* global variables */

#define NLMAX 4
int nepoch = 0;
int NepochTot = 0;
int lprint = 0;
int lbest = 0;
int ibest = 0;
float ebest = 9999999;
int Npat[2]={0,0},Ninput[2],Noutput[2],Pond[2];
int netdefined = 0;
int learndefined = 0;
int examplesdefined = 0;
char chinputs[1024] = {'\0'};
int Nweights = 0;
int Nlayer = 0, Nneur[NLMAX];
int LearnReset = 50, LearnMethod = 6;
float LearnPar = 0.1, LearnDecay = 0.992, LearnMomentum = 0.1;
float LearnFlatSpot = 0, LearnTau = 1.5;
float LearnLambda = 1;
float ***Weights;

static char vidmlp[] = 
"@(#)MLPfit 1.40     January 2000";

/* "subroutines" */

void MLP_Error(char *s1, char *s2);
void MLP_PatSet(int ifile, char *ch, int *idn);
void MLP_prpawf(char *chinput, char *filename, int idn);
void MLP_WritePat(int ifile, char *filename);
void MLP_BookWeights();
void MLP_FreeWeights();
void MLP_StoreWeights();
void MLP_BestWeights();

char *mlpvers()
{
	return &vidmlp[4];
}

/****************************************************************/
/* mlppawc                                                      */
/*                                                              */
/* main routine for MLP commands within PAW                     */
/*                                                              */
/* J.Schwindling 	18-MAY-99				*/
/* 	commands implemented are: 	MLP/CREATE		*/
/*					MLP/RESET		*/
/*					MLP/LEARN		*/
/*					MLP/LMET		*/
/*					MLP/WEIGHTS/WRITE	*/
/*					MLP/WEIGHTS/READ	*/
/*					MLP/L(T)PAT/SET		*/
/*					MLP/L(T)PAT/READ	*/
/*					MLP/L(T)PAT/WRITE	*/
/*								*/
/* Modified by J.Schwindling 09-JUN-99  MLP/STATUS		*/
/*             J.Schwindling 22-NOV-99  0 hidden layer          */ 
/*								*/
/****************************************************************/

void mlppawc()
{
	char		*cmd_path;
	char		*cmd_string;
	char 		*cmd_char;
	char		*filename;
	char s[1024];
        int i, j, nr;
	static int idn;
        char *cc[NLMAX];
	float rmax, rmin, rmaxdep, redfact=0.98;
	float err, rmax1, rmax2, rmin1, rmin2, x[2], y[2], par1, par2, par3;
	float *error, *error2, *xx;
	int idummy, idummy1, idummy2, idummy3, ifile, iepoch, iiepoch, lastupd;
	int lplot;
	int one = 1;
	int zero = 0;
	
	cmd_path = str_new( ku_path() );
	cmd_string = strrchr( cmd_path, '/' );
	if ( cmd_string != 0 ) {
		cmd_string += 1;
	} else {
		cmd_string = cmd_path;
	}
	
	if(strcasecmp(cmd_string,"CREATE") == 0)	/* MLP/CREATE */
		{
		Nlayer = 0;
		Nweights = 0;	
		Nneur[0] = ku_geti(); 
		Nneur[1] = ku_geti();
		Nneur[2] = ku_geti();
		Nneur[3] = ku_geti();
		Nlayer = 4;
		if(Nneur[2] == 0)
			{
			Nlayer = 3;
			Nneur[2] = Nneur[3];
			}		
		if(Nneur[1] == 0)
			{
			Nlayer = 2;
			Nneur[1] = Nneur[3];
			}					
		Nweights = 0;	
		for(i=1;i<Nlayer;i++)
			{
			Nweights += (Nneur[i-1]+1)*Nneur[i];
			}

/* set the MLP structure */
		mlplearnfree_();
		mlp_setnet(Nlayer,Nneur);
		netdefined = 1;
		mlplearnalloc_();
		mlp_initw(one);
		NepochTot = 0;
		}
	else if(strcasecmp(cmd_string,"RESET") == 0)	/* MLP/RESET */
		{
		if(netdefined!=0) mlp_free();
		netdefined = 0;
		learndefined = 0;
		examplesdefined = 0;
		Npat[0] = 0;
		Npat[1] = 0;
		}
	else if(strcasecmp(cmd_path,"/MLP/STATUS") == 0) /* MLP/STATUS */
		{
                printf("\n**********************************************\n");
                printf(  "*     MLPfit version %.2f    January 2000\n",
				mlpversion_());
		printf(  "*                                \n");
		if(netdefined == 0)
			{
			printf(  "*     no network defined         \n");
			printf(  "*                                \n");
			}
		else
			{
			printf(  "*     network with %d layers     \n",Nlayer);
			printf(  "*     of ");
			for(i=0;i<Nlayer;i++)
				{
				printf("%d ",Nneur[i]);
				}
			printf("neurons\n");	
			printf(  "*                                \n");
			}
		nr = LearnReset;	
		if(learndefined == 0)
			{
			printf(  "*     no learning method defined \n");
			printf(  "*     default will be: \n");
			printf(  "*                                \n");
			nr = Nweights;
			}
		switch(LearnMethod)
			{
			case 1:
				printf("*     Stochastic Minimization \n");
				printf("*     \tlearning parameter = \t %f\n",
					LearnPar);
				printf("*     momentum term = \t %f\n",
					LearnMomentum);
				printf("*     \tdecay = \t\t %f\n",
					LearnDecay);
				break;
			case 2:
				printf("*     Fixed Steps Steepest Descent \n");		
				printf("*     \tlearning parameter = \t %f\n",
					LearnPar);
				printf("*     \tmomentum term = \t %f\n",
					LearnMomentum);
				printf("*     \tdecay = \t\t %f\n",
					LearnDecay);
				break;
			case 3:
				printf("*     Steepest Descent \n");
				printf("*     \tline search precision = \t %f\n",
					LearnTau);	
				break;
			case 4:
			printf("*     Fletcher-Reeves Conjugate Gradients\n");	
				printf("*     \tline search precision = \t %f\n",
					LearnTau);	
				printf("*     \treset every %d epochs\n",nr);	
				break;
			case 5:
			printf("*     Ribiere-Polak Conjugate Gradients\n");	
				printf("*     \tline search precision = \t %f\n",
					LearnTau);	
				printf("*     \treset every %d epochs\n",nr);	
				break;
			case 6:
			printf("*     Broyden-Fletcher-Goldfarb-Shanno\n");	
				printf("*     \tline search precision = \t %f\n",
					LearnTau);	
				printf("*     \treset every %d epochs\n",nr);	
				break;
			case 7:
			printf("*     Hybrid BFGS-linear\n");	
				printf("*     \tline search precision = \t %f\n",
					LearnTau);	
				printf("*     \treset every %d epochs\n",nr);	
				printf("*     \tregularization term = %f\n",
					LearnLambda);
				break;
			}		
		printf(  "*                                \n");
		if(examplesdefined == 0)
			{
			printf(  "*     no examples defined        \n");
			printf(  "*                                \n");
			}
		else
			{
			printf(  "*     %d learning examples\n",Npat[0]);
			printf(  "*     %d test examples\n",Npat[1]);
			printf(  "*                                \n");
			}
		printf("*     Network has been trained for %d epochs\n",
			NepochTot);				
			printf(  "*                                \n");
                printf("**********************************************\n");
		}
	else if(strcasecmp(cmd_path,"/MLP/LEARN") == 0)
		{
		if(Npat[0]==0) 
			{
			MLP_Error(cmd_path,"no examples defined");
			return;
			}
		if(netdefined == 0)
			{
			MLP_Error(cmd_path,"no network defined");
			return;
			}
		if(learndefined == 0)	/* set default learning */
			{
			mlp_setlearn(LearnMethod, LearnPar, LearnDecay,
				     LearnMomentum, LearnFlatSpot,
				     Nweights, LearnTau, LearnLambda);	
			learndefined = 1;	     			
			}					
		nepoch = ku_geti();
		cmd_char = strdup( ku_getc() );
		filename = str_tolower(strdup( ku_getc() ));
		if(strchr(cmd_char,'+')==0) 
			{
			if(strchr(cmd_char,'I')==0)
				{
				mlp_initw(one);
				}
			else
				{
				mlp_initw(zero);
				}	
			NepochTot = 0;
			}
		lprint = 1;
		if(strchr(cmd_char,'Q') != 0) lprint = 0;
		lplot = 1;
		if(strchr(cmd_char,'N') != 0) lplot = 0;
		lbest = 0; 
		if(strchr(cmd_char,'B') != 0 && Npat[1] != 0) 
			{
			lbest = 1;
			ebest = 9999999;
			ibest = 0;
			MLP_BookWeights();
			}
		
		if(HEXIST(2000000)) HDELET(2000000);
		HBOOK1(2000000,"MLP error",nepoch,0.,nepoch,0.);
		if(Npat[1]!=0) 
			{
		        if(HEXIST(2000001)) HDELET(2000001);
			HBOOK1(2000001,"MLP error",nepoch,0.,nepoch,0.);			
			}
		error = (float *) malloc(nepoch*sizeof(float));	
		error2 = (float *) malloc(nepoch*sizeof(float));
		xx = (float *) malloc(nepoch*sizeof(float));
			
		lastupd = -1000;
		redfact = 0.98;	
		for(iepoch=0; iepoch<nepoch; iepoch++)  /* loop on epochs */
 			{
			iiepoch = iepoch+1;
			err = mlp_epoch(iiepoch);
			err = err/(float) Npat[0];	    
			err = sqrt(err);
			error[iepoch] = err;
			if(Npat[1]!=0) 
				{
				ifile = 1;
				error2[iepoch] = 
					sqrt (mlptest_(&ifile)/(float) Npat[1]);
				if(lbest == 1 && error2[iepoch] < ebest)
					{
					ibest = iiepoch;
					ebest = error2[iepoch];
					MLP_StoreWeights();
					}
				}
				
/* determine scale and plot histogram */
			if(iepoch==0 && lplot>0)	
				{
				ifile = 0;
				err = sqrt (mlptest_(&ifile)/(float) Npat[0]);
				rmax1 =  1.02*err;
				rmin1 =  0.98*err;
				rmax = rmax1;
				rmin = rmin1;
				if(Npat[1]!=0)
					{
					err = error2[iepoch];
					rmax2 =  1.02*err;
					rmin2 = 0.98*err;
					if(rmax2 > rmax) rmax = rmax2;
					if(rmin2 < rmin) rmin = rmin2;
					}
				HMAXIM(2000000,rmax);
				HMINIM(2000000,rmin);
				HPLOT(2000000," "," ",0);
				rmaxdep = rmax;
				}

/* change scales of plot */
			if(error[iepoch]<rmin && lplot>0)  
				{
				if(lastupd==iepoch-1)
					{
					redfact = redfact*redfact;
					}
				lastupd = iepoch;
				rmin = error[iepoch]*redfact;
				HMINIM(2000000,rmin);
				HPLOT(2000000," "," ",0);
				for(j=1;j<=iepoch+1;j++)
					{
					xx[j-1] = (float) j;
					}
				IPL(iepoch,xx,error);
				if(Npat[1]!=0)
					{
					ISLN(2);
					IPL(iepoch,xx,error2);
					ISLN(1);
					}
				IUWK(0,2);	
				}
	
			if(Npat[1]!=0 && lplot>0)
				{
				if(error2[iepoch]<rmin)
					{
					if(lastupd==iepoch-1)
						{
						redfact = redfact*redfact;
						}
					lastupd = iepoch;
					rmin = error2[iepoch]*redfact;	
					HMINIM(2000000,rmin);
					HPLOT(2000000," "," ",0);
					for(j=1;j<=iepoch+1;j++)
						{
						xx[j-1] = (float) j;
						}
					IPL(iepoch,xx,error);
					if(Npat[1]!=0)
						{
						ISLN(2);
						IPL(iepoch,xx,error2);
						ISLN(1);
						}
					IUWK(0,2);	
					}
				}
/* plot learning curve */
			if(iepoch>0 && lplot>0) 	
				{
				x[0] = (float) (iepoch);
				x[1] = (float) (iepoch+1);
				y[0] = error[iepoch-1];
				y[1] = error[iepoch];
				IPL(2,x,y);
				if(Npat[1]!=0)	
					{
					y[0] = error2[iepoch-1];
					y[1] = error2[iepoch];
					ISLN(2);
					IPL(2,x,y);
					ISLN(1);
					}
				IUWK(0,2);
				}
			if(lprint != 0 || iepoch==nepoch-1)	/* print error on screen */
				{
				if(Npat[1]==0)
					{
					printf("Iepoch %d \t Error %e\n",
						iepoch+1,error[iepoch]);
					}
				else
					{
					printf("Iepoch %d \t Error %e\t test %e\n",
						iepoch+1,error[iepoch],
						error2[iepoch]);
					}	
				}					
			}
		if(lbest == 1)
			{
			printf("\nUsing the weights from epoch %d\n",ibest);
			MLP_BestWeights();
			MLP_FreeWeights();
			err = error[ibest - 1];
			}
		if(lbest == 1)
			{
			NepochTot += ibest;
			}
		else
			{
			NepochTot += nepoch;
			}		
		MLP_prpawf(chinputs,filename,idn);
		printf("\n");
	printf("*** chi2 = %e for %d - %d degrees of freedom\n\n",
	err*err*(float) Npat[0],Npat[0],Nweights);
		HMAXIM(2000000,rmaxdep);
		HPAK(2000000,error);	
		if(Npat[1]!=0) 
			{
			HPAK(2000001,error2);						
			}
		free(error);
		free(error2);
		free(xx);	
		}
	else if(strcasecmp(cmd_path,"/MLP/LMET") == 0)
		{
		LearnMethod = ku_geti();
		par1 = (float) ku_getr();
		par2 = (float) ku_getr();
		par3 = (float) ku_getr();
		LearnFlatSpot = 0.;
		if(LearnMethod == 1 || LearnMethod == 2) /* stochastic or fixed
							 step steepest descent */	
			{
			if(par1<0) 
				{
				LearnPar = 0.1;
				}
			else 
				{
				LearnPar = par1;
				}	
			if(par2<0) 
				{
				LearnMomentum = 0;
				}
			else
				{
				LearnMomentum = par2;
				}	
			if(par3<0) 
				{
				LearnDecay = 1;
				}
			else
				{
				LearnDecay = par3;
				}	
			LearnReset = 1000;
			LearnTau = 3;
			LearnLambda = 1;
			}
		else
			{
			if(par1<0) 
				{
				LearnReset =1000;
				}
			else
				{
				LearnReset = (int) par1;
				}
			if(par2<0)
				{
				LearnTau = 1.5;
				}
			else
				{
				LearnTau = par2;
				}	
			if(par3<0)
				{
				LearnLambda = 1;
				}
			else
				{
				LearnLambda = par3;
				}	
			LearnPar = 0.1;
			LearnDecay = 1;
			LearnMomentum = 0;
			}
		mlp_setlearn(LearnMethod,LearnPar,LearnDecay,LearnMomentum,
			     LearnFlatSpot,LearnReset,LearnTau,LearnLambda); 
		learndefined = 1;	     
		}
	else if(strcasecmp(cmd_path,"/MLP/WEIGHTS/WRITE") == 0)
		{
		cmd_char = str_tolower(strdup( ku_getc() ));
		SaveWeights(cmd_char,NepochTot);
		}	
	else if(strcasecmp(cmd_path,"/MLP/WEIGHTS/READ") == 0)
		{
		cmd_char = str_tolower(strdup( ku_getc() ));
		LoadWeights(cmd_char,&NepochTot);
		}	
	else if(strcasecmp(cmd_path,"/MLP/LPAT/SET") == 0)
		{
		MLP_PatSet(0,chinputs,&idn);
		}
	else if(strcasecmp(cmd_path,"/MLP/TPAT/SET") == 0)
		{
		MLP_PatSet(1,chinputs,&idn);
		}
	else if(strcasecmp(cmd_path,"/MLP/LPAT/READ") == 0)
		{
		cmd_char = str_tolower(strdup( ku_getc() ));
		ReadPatterns(cmd_char,0,&netdefined,&learndefined,
				&examplesdefined);
		ifile = 0;
		mlpgetnpat_(&ifile,&(Npat[ifile]),&(Ninput[ifile]),
			    &Noutput[ifile],&Pond[ifile]);
		}
	else if(strcasecmp(cmd_path,"/MLP/LPAT/WRITE") == 0)
		{
		cmd_char = str_tolower(strdup( ku_getc() ));
		MLP_WritePat(0,cmd_char);
		}		
	else if(strcasecmp(cmd_path,"/MLP/TPAT/WRITE") == 0)
		{
		cmd_char = str_tolower(strdup( ku_getc() ));
		MLP_WritePat(1,cmd_char);
		}		
	else if(strcasecmp(cmd_path,"/MLP/TPAT/READ") == 0)
		{
		cmd_char = str_tolower(strdup( ku_getc() ));
		ReadPatterns(cmd_char,1,&idummy1,&idummy2,&idummy3);
		ifile = 1;
		mlpgetnpat_(&ifile,&(Npat[ifile]),&(Ninput[ifile]),
			    &Noutput[ifile],&Pond[ifile]);
		}	
	else					/* should not happen */
		{
		printf("Command not existing or not implemented\n");
		}
}


/****************************************************************/
/* MLP_BookWeights						*/
/*								*/
/* allocate memory for the best weights				*/
/*								*/
/* J.Schwindling 	20-OCT-00				*/
/****************************************************************/

void MLP_BookWeights()
{
	int ilayer, ineur;
	
	Weights = (float ***) malloc((Nlayer-1)*sizeof(float**));
	for(ilayer=1; ilayer<Nlayer; ilayer++)
		{
		Weights[ilayer-1] = (float **) malloc(Nneur[ilayer]*
						sizeof(float *));
		for(ineur=0; ineur<Nneur[ilayer]; ineur++)
			{
			Weights[ilayer-1][ineur] = (float *)
				malloc((Nneur[ilayer-1]+1) * sizeof(float));
			}
		}
}


/****************************************************************/
/* MLP_FreeWeights						*/
/*								*/
/* frees memory for the best weights				*/
/*								*/
/* J.Schwindling 	20-OCT-00				*/
/****************************************************************/

void MLP_FreeWeights()
{
	int ilayer, ineur;
	
	for(ilayer=1; ilayer<Nlayer; ilayer++)
		{
		for(ineur=0; ineur<Nneur[ilayer]; ineur++)
			{
			free(Weights[ilayer-1][ineur]);
			}
		}
		 
	for(ilayer=1; ilayer<Nlayer; ilayer++)
		free(Weights[ilayer-1]);

	free(Weights);	
}


/****************************************************************/
/* MLP_StoreWeights						*/
/*								*/
/* stores the best weights					*/
/*								*/
/* J.Schwindling 	20-OCT-00				*/
/****************************************************************/

void MLP_StoreWeights()
{
	int ilayer, ineur, ierr;
	
	for(ilayer=2; ilayer<=Nlayer; ilayer++)
		{
		for(ineur=1; ineur<=Nneur[ilayer]; ineur++)
			ierr = mlpgetw_(&ilayer,&ineur,Weights[ilayer-2][ineur-1]);
		}
}


/****************************************************************/
/* MLP_BestWeights						*/
/*								*/
/* resets the weights to the best ones				*/
/*								*/
/* J.Schwindling 	20-OCT-00				*/
/****************************************************************/

void MLP_BestWeights()
{
	int ilayer, ineur, ierr;
	
	for(ilayer=2; ilayer<=Nlayer; ilayer++)
		{
		for(ineur=1; ineur<=Nneur[ilayer]; ineur++)
			ierr = mlpsetw_(&ilayer,&ineur,Weights[ilayer-2][ineur-1]);
		}
}


/****************************************************************/
/* MLP_Error							*/
/*								*/
/* prints an error message					*/
/*								*/
/* J.Schwindling 	18-MAY-99				*/
/****************************************************************/

void MLP_Error(char *s1, char *s2)
{
	printf(" *** %s: %s\n",s1,s2);
}


/****************************************************************/
/* MLP_Set1dPat							*/
/*								*/
/* sets the examples from a 1d histogram			*/
/*								*/
/* inputs: int ifile = 0: learning examples			*/
/*		     = 1: test examples				*/
/* 	   int idn: histogram number				*/
/*	   int ipond: example weighting (1) or not (0)		*/
/*								*/
/* return value (int) = number of examples			*/
/*								*/
/* J.Schwindling 	28-MAY-99				*/
/* Modified:		08-JUN-99 example weighting 		*/
/****************************************************************/

int MLP_Set1dPat(int ifile, int idn, int ipond)
{
	char chtitl[80];
	int nx, ny, loc, nwt, ier;
	float xmi, xma, ymi, yma, dbin;
	float x;
        float *y, *ey, pond;	
	int idummy, i;
	float rdummy;
	
	HGIVE(idn,chtitl,nx,xmi,xma,ny,ymi,yma,nwt,loc);
	y = (float *) malloc(nx*sizeof(float));
	ey = (float *) malloc(nx*sizeof(float));
	
	ier = mlp_setnpat(ifile,nx,ipond,1,1);
	if(ier!=0)
		{
		MLP_Error("MLP/LPAT/SET",
			  "Error while setting number of patterns");
		free(ey);
		free(y);	  
		return(-1);	  
		}
	dbin = (xma - xmi) / (float) nx;
	HUNPAK(idn,y,"    ",1);
	HUNPKE(idn,ey,"    ",1);
	for(i=0;i<nx;i++)
		{
		x = xmi + ((float) i + 0.5) * dbin;
		rdummy = 1;
		idummy = i+1;
		if(ipond==0)
			{
			pond = 1;
			}
		else
			{	
			if(ey[i]>0.0000001) pond = 1/(ey[i]*ey[i]);
			else pond = 1;
			}
		ier = mlp_setpat(ifile,idummy,&x,&(y[i]),pond);
		}
	free(ey);
	free(y);	
	return nx;	
}


/****************************************************************/
/* MLP_Set2dPat							*/
/*								*/
/* sets the examples from a 2d histogram			*/
/*								*/
/* inputs: int ifile = 0: learning examples			*/
/*		     = 1: test examples				*/
/* 	   int idn: histogram number				*/
/*	   int ipond: example weighting (1) or not (0)		*/
/*								*/
/* return value (int) = number of examples			*/
/*								*/
/* J.Schwindling 	28-MAY-99				*/
/* Modified:		08-JUN-99 example weighting 		*/
/****************************************************************/

int MLP_Set2dPat(int ifile, int idn, int ipond)
{
	char chtitl[80];
	int nx, ny, loc, nwt, ier;
	float xmi, xma, ymi, yma, dbinx, dbiny;
	float x[2], y, ey, pond;
	int idummy, idummy2, i, j;
	float rdummy;
	
	HGIVE(idn,chtitl,nx,xmi,xma,ny,ymi,yma,nwt,loc);
	idummy = 0;
	idummy2 = nx*ny;
	ier = mlp_setnpat(ifile,idummy2,ipond,2,1);
	if(ier!=0)
		{
		MLP_Error("MLP/LPAT/SET",
			  "Error while setting number of patterns");
		return(-1);	  
		}
	dbinx = (xma - xmi) / (float) nx;
	dbiny = (yma - ymi) / (float) ny;
	for(i=1;i<=nx;i++)
		{
		x[0] = xmi + ((float) i - 0.5) * dbinx;
		for(j=1;j<=ny;j++)
			{
			x[1] = ymi + ((float) j - 0.5) * dbiny;
			y = HIJ(idn,i,j);
			ey = HIJE(idn,i,j);
			if(ipond==0)
				{
				pond = 1;
				}
			else
				{	
				if(ey>0.0000001) pond = 1/(ey*ey);
				else pond = 0;
				}
			idummy = (i-1)*ny+j;
			rdummy = 1;
			ier = mlp_setpat(ifile,idummy,x,&y,pond);
			}
		}
			
	return(nx*ny);	
}


/****************************************************************/
/* MLP_count							*/
/*								*/
/* counts the number of items separated by %			*/
/*								*/
/* input: char *s = input string				*/
/* return value (int) = number of items				*/
/*								*/
/* J.Schwindling 	31-MAY-99				*/
/****************************************************************/

int MLP_count(char *s)
{
  char tmp[1024];
  int i=0;
  
  strcpy(tmp,s);
  if (strtok(tmp,"%"))
    {
      i=1;
      while (strtok(NULL,"%")) i++;
    }
  return i;
}


/****************************************************************/
/* MLP_getnItems						*/
/*								*/
/* gets n items separated by %					*/
/*								*/
/* input:	int n = number of items				*/ 
/*		char *s = input string				*/
/* output:	char **ss = returned items			*/
/*								*/
/* J.Schwindling 	31-MAY-99				*/
/****************************************************************/

void MLP_getnItems(int n, char *s, char **ss)
{
  char tmp[1024];
  int i;     
  strcpy(tmp,s);
  if (n>0)
    {
      strcpy(ss[0],strtok(tmp,"%"));
      for (i=1;i<n;i++)
        strcpy(ss[i],strtok(NULL,"%"));
    }
}


/****************************************************************/
/* MLP_WritePat							*/
/*								*/
/* to write the examples in an ASCII file			*/
/*								*/
/* input: int ifile = 0: learning examples			*/
/*		    = 1: test examples				*/
/*	  char *filename: name of the output file		*/
/*								*/
/* J.Schwindling 	31-MAY-99				*/
/****************************************************************/

void MLP_WritePat(int ifile, char *filename)
{
	FILE *W;
	int ipat, i, npat, nin, nout, ipond;
	float *rin, *rout, *rans, pond;
	
	if(Npat[ifile] == 0) 
		{
		MLP_Error("MLP/L(T)PAT/WRITE","no example in memory");
		return;
		}
		
	mlpgetnpat_(&ifile, &npat, &nin, &nout, &ipond); 
	rin = (float *) malloc(nin*sizeof(float));
	rout = (float *) malloc(nout*sizeof(float));
	rans = (float *) malloc(nout*sizeof(float));
		
	W=fopen(filename,"w");
	fprintf(W,"# example file generated by the PAW-MLPfit interface\n");
	fprintf(W,"NPAT %d\n",npat);
	fprintf(W,"NINP %d\n",nin);
	fprintf(W,"NOUT %d\n",nout);
	fprintf(W,"POND %d\n",ipond);	
	for(ipat=1; ipat<=Npat[ifile]; ipat++)
		{
		mlpgetdata_(&ifile,&ipat,rin,rout,rans,&pond);
		for(i=0;i<nin;i++)
			{
			fprintf(W,"%le ",(double) rin[i]);
			}
		fprintf(W,"\n");
		for(i=0;i<nout;i++)
			{
			fprintf(W,"%le ",(double) rans[i]);
			}
		if(ipond == 0)
			{
			fprintf(W,"\n");
			}
		else	
			{
			fprintf(W,"%le\n",(double) pond);
			}
		}	
	fclose(W);
	free(rans);
	free(rout);
	free(rin);
}


/****************************************************************/
/* MLP_PatSet							*/
/*								*/
/* to set the learning or test patters from a 1d, 2d histogram  */
/* or from an ntuple						*/
/*								*/
/* input: int ifile = 0: learning examples			*/
/*		    = 1: test examples				*/
/* output: char *chins = string containing the input variables  */
/*			 in case of ntuple, null string otherw. */ 
/*								*/
/* J.Schwindling 	28-MAY-99				*/
/* Modified:		08-JUN-99 example weighting for 1d or	*/
/*				  2d histograms			*/
/****************************************************************/

void MLP_PatSet(int ifile, char *chins, int *id)
{
	char *chid, *chid2, *chid3, chpath[80],ch2[10],ch3[10],chline[1024];
	char *chinput, *choutput, *chweight, *chcuts, *chpar, *chcom;
	int lengthcuts, nevt, ifirst, iadd, ninputs, noutputs;
	int idn, np=0, ierr, nentries, naddpat, icount, ipat, i, ipond=0;
	int kind[32];
	FILE *TMPfile;
	char **ss;
	float *rin, *rans, weight;
	int item, nitems;
	int zero=0, one=1;
	
	chins[0] = '\0';
	chid = strdup( ku_getc() );
	if(strchr(chid,'/')==0)		/* only histo number, no //lun... */
		{
		sscanf(chid,"%d",&idn);
		}
	else
		{
		chid2 = strdup( chid+2 );
		chid3 = strdup( strchr(chid2,'/')+1 );
		sscanf(chid3,"%d",&idn);
		}
	HRIN(idn,9999,0);
	*id = idn;
	HKIND(idn,kind," ");	/* gets type of histogram */
	if(kind[0]==1 || kind[0]==8) 	/* 1d plot */
		{
		chpar = strdup(ku_getc());
		if(strchr(chpar,'E') || strchr(chpar,'e')) ipond=1;
		np = MLP_Set1dPat(ifile,idn,ipond);
		Npat[ifile] = np;
		}
	else if(kind[0]==2)		/* 2d plot */
		{
		chpar = strdup(ku_getc());
		if(strchr(chpar,'E') || strchr(chpar,'e')) ipond=1;
		np = MLP_Set2dPat(ifile,idn,ipond);
		Npat[ifile] = np;
		}
	else if(kind[0]==4)		/* Ntuple */
		{
		
/* getting command line */		
		chinput = strdup(ku_getc());
		if(chinput[0] == '\0') 
			{
			MLP_Error("MLP/LPAT/SET",
			"a list of input variables is required");
			return;
			}
		choutput = strdup(ku_getc());
		if(choutput[0] == '\0') 
			{
			MLP_Error("MLP/LPAT/SET",
			"a list of output variables is required");
			return;
			}
		chweight = strdup(ku_getc());
		nevt = ku_geti();
		ifirst = ku_geti();
		chcuts = strdup(ku_getc());
		chpar = strdup(ku_getc());
		lengthcuts = strlen(chcuts);
		if(lengthcuts == 0) 
			{
			chcuts = "!";
			}
		iadd = 1;
		if(strchr(chpar,'+')==0) iadd = 0;

/* setting command for ku_exec */		
		sprintf(ch2," %d ",nevt);	
		sprintf(ch3," %d ",ifirst);
		chcom = (char *) malloc(1024*sizeof(char));
		chcom[0] = '\0';
		chcom = strcat(chcom,"NT/DUMP ");
		chcom = strcat(chcom,chid);
		chcom = strcat(chcom,".");
		chcom = strcat(chcom,chinput);
		chcom = strcat(chcom,"%");
		chcom = strcat(chcom,choutput);
		chcom = strcat(chcom,"%");
		chcom = strcat(chcom,chweight);
		chcom = strcat(chcom," ");
		chcom = strcat(chcom,chcuts);
		chcom = strcat(chcom,ch2);
		chcom = strcat(chcom,ch3);
		chcom = strcat(chcom," tmp.data");
		ierr = ku_exec(chcom);
		free(chcom);
		if(ierr != 0) 
			{
			MLP_Error("MLP/LPAT/SET", 
			"problem writing examples in temporary file");
			return;
			}
			
/* number of inputs/outputs	*/		
		ninputs = MLP_count(chinput);
		noutputs = MLP_count(choutput);
		rin = (float *) malloc(ninputs*sizeof(float));
		rans = (float *) malloc(noutputs*sizeof(float));
		nitems = ninputs+noutputs+1;
		ss = (char **) malloc(nitems*sizeof(char *));
		for(i=0; i<nitems; i++)
			{
			ss[i] = (char *) malloc(30*sizeof(char));
			}
		
		HNOENT(idn,nentries);
		naddpat = nevt;
		if(nentries-ifirst+1 < nevt) naddpat = nentries-ifirst+1;

/* read temporary file */
		TMPfile = fopen("tmp.data","r");
		if(iadd == 0) Npat[ifile] = 0;		
		while(fgets(chline,1024,TMPfile))
			{
			if(chline[0] != '%')
				{
				np++;
				Npat[ifile]++;
				MLP_getnItems(nitems,chline,ss);
				item = 0;
				for(i=0;i<ninputs;i++)
					{
					sscanf(ss[item],"%f",&rin[i]);
					item++;
					}
				for(i=0;i<noutputs;i++)
					{
					sscanf(ss[item],"%f",&rans[i]);
					item++;
					}
				sscanf(ss[item],"%f",&weight);
				ierr = mlpaddnpat_(&ifile,&one,&one,
					&ninputs, &noutputs);
					
				ierr = mlp_setpat(ifile, Npat[ifile], 
						  rin, rans, weight);
						  	
				}
			}
		fclose(TMPfile);
		for(i=0; i<ninputs+noutputs+1; i++)
			{
			free(ss[i]);
			}
		free(ss);
		free(rin);
		free(rans);
		chins = strcpy(chins,chinput);
		if(iadd == 1) 	printf("\n %d examples added\n",np);
		else		printf("\n %d examples loaded\n",np);
		}		
	else if(kind[0]==0)
		{
		MLP_Error("MLP/LPAT/SET","unknown histogram");
		}
	else if(kind[0]==-1)
		{
		MLP_Error("MLP/LPAT/SET","unknown type of histogram");
		}
	else
		{
		MLP_Error("MLP/LPAT/SET",
			  "histogram type not compatible with MLP/SET/LPAT");
		}
		
	if(ifile == 0) examplesdefined = 1;					
}


/***********************************************************/
/* MLP_prpawf                                              */
/*                                                         */
/* prints out the function pawmlp.f  which can be used     */
/*					        in PAW     */
/*                                                         */
/* Author: J.Schwindling   31-May-99                       */
/***********************************************************/   

void MLP_prpawf(char *chinput, char *filename, int idn)
{
	int il, in, jn, j, ninputs;
	FILE *W;
	char **ss;
	char s[80], file[80], *stmp;
	int nlayer, nneur[5], ierr, ilp1, inp1, tfunc;
	float weights[101]; 

	strcpy(file,filename);
	W=fopen(filename,"w");
        strcpy(s,strtok(filename,"."));
	
	mlpgetnet_(&nlayer, nneur);
	if(nneur[0]==1 && chinput[0]=='\0') 
		{
		fprintf(W,"      REAL FUNCTION %s(X)\n",s);
		}
	else if(nneur[0]==2 && chinputs[0]=='\0') 
		{
		fprintf(W,"      REAL FUNCTION %s(X,Y)\n",s);
		}
	else
		{
		fprintf(W,"      REAL FUNCTION %s(XDUMMY)\n",s);
		}	
	fprintf(W,"C\n");
	fprintf(W,"C    Function generated by the PAW - MLPfit interface\n");
	fprintf(W,"C\n");
	
	if(nneur[0]==1 && chinput[0]=='\0') 
		{
		fprintf(W,"      OUT1 = X\n");
		}
	else if(nneur[0]==2 && chinput[0]=='\0') 
		{
		fprintf(W,"      OUT1 = X\n");
		fprintf(W,"      OUT2 = Y\n");
		}
	else
		{
		fprintf(W,"      include ?\n");
		
		fprintf(W,"C\n");
		ninputs = MLP_count(chinput);
		ss = (char **) malloc(ninputs*sizeof(char *));
		for(in=0; in<ninputs; in++)
			{
			ss[in] = (char *) malloc(256*sizeof(char));
			}
		MLP_getnItems(ninputs,chinput,ss);
		for(in=0; in<ninputs; in++)
			{
			if(strlen(ss[in]) < 50)
				{
				fprintf(W,"      OUT%d = %s\n",
					in+1,ss[in]);
				}
			else
				{
				stmp = (char *)
					malloc(51*sizeof(char));
				fprintf(W,"      OUT%d = ",in+1);
				for(j=0; j<(int) (strlen(ss[in])/50);
				    j++)
				    	{
					stmp = strncpy(stmp, 
						(char *) (ss[in]+50*j),
						50);
					stmp[50] = '\0';
					fprintf(W,"%s\n",stmp);
					fprintf(W,"     >");	
					}
				stmp = strncpy(stmp,
					(char *) (ss[in]+j*50),50);	
				fprintf(W,"%s\n",stmp);	
				free(stmp);
				}		
			free(ss[in]);
			}
		free(ss);	
		}	
	
	for(il=1; il<nlayer-1; il++)
		{
		ilp1 = il+1;
		fprintf(W,"C\n");
		fprintf(W,"C     layer %d\n",il+1);
		for(in=0; in<nneur[il]; in++)
			{
			inp1 = in+1;
			ierr = mlpgetw_(&ilp1,&inp1,weights);
			fprintf(W,"      RIN%d = %le\n",in+1,
					(double) weights[0]);
			for(jn=1;jn<=nneur[il-1]; jn++)
				fprintf(W,"     > +(%le) * OUT%d\n",
					(double) weights[jn],jn);
			}
		fprintf(W,"C\n");
		for(in=0; in<nneur[il]; in++)
			{
			inp1 = in+1;
			tfunc = 2;
			if(tfunc==0) 
				{
				fprintf(W,"      OUT%d = 0\n",in+1);
				}
			else if(tfunc==1)
				{
				fprintf(W,"      OUT%d = RIN%d\n",in+1,in+1);
				}
			else if(tfunc==2)
				{
				fprintf(W,"      OUT%d = SIGMOID(RIN%d)\n",
					in+1,in+1);
				}
			}
		}
	il = nlayer-1;
	ilp1 = il+1;
	inp1 = 1;
	ierr = mlpgetw_(&ilp1,&inp1,weights);
	fprintf(W,"C\n");
	fprintf(W,"C     layer %d\n",il+1);
	fprintf(W,"      %s = %le\n",s,
			(double) weights[0]);
	for(jn=1; jn<=nneur[il-1]; jn++)
		fprintf(W,"     > +(%le) * OUT%d\n",
			(double) weights[jn],jn);
		
	fprintf(W,"C\n");
	fprintf(W,"      END\n");
	fprintf(W,"      REAL FUNCTION SIGMOID(X)\n");
	fprintf(W,"      IF(X.GT.37.) THEN\n");
	fprintf(W,"         SIGMOID = 1.\n");
	fprintf(W,"      ELSE IF(X.LT.-37.) THEN\n");
	fprintf(W,"         SIGMOID = 0.\n");
	fprintf(W,"      ELSE\n");
	fprintf(W,"         SIGMOID = 1./(1.+EXP(-X))\n");
	fprintf(W,"      ENDIF\n");
	fprintf(W,"      END\n");
	
	fclose(W);
}
