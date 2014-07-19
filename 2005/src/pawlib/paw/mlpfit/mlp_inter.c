#include "paw/pilot.h"
#include <math.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>

/* #define MLPHEADER */
#include "mlp_gen.h"

/*  This file contains routines which can be called for fortran or C code 
    Their name follow the rules:
    - only lowercase characters
    - start with mlp
    - end with _ for being compatible with fortran calling 
    		-> to call them from fortran use the name WITHOUT _
		-> to call them from C use the name WITH _
    - all arguments passed by address		
    
    J.Schwindling 14-NOV-98
*/

#if defined(CERNLIB_QX_SC)
#define mlpsetnet mlpsetnet_
#define mlpsetlearn mlpsetlearn_
#define mlpsetnpat mlpsetnpat_
#define mlpsetpat mlpsetpat_
#define mlpinitw mlpinitw_
#define mlpepoch mlpepoch_
#define mlpfree mlpfree_
#define mlpprpawf mlpprpawf_
#define mlpcompute mlpcompute_
#endif

#if defined(CERNLIB_QXCAPT)
#define mlpsetnet MLPSETNET
#define mlpsetlearn MLPSETLEARN
#define mlpsetnpat MLPSETNPAT
#define mlpsetpat MLPSETPAT
#define mlpinitw MLPINITW
#define mlpepoch MLPEPOCH
#define mlpfree MLPFREE
#define mlpprpawf MLPPRPAWF
#define mlpcompute MLPCOMPUTE
#endif

#if defined(CERNLIB_IBM)
#pragma linkage (mlpsetnet ,FORTRAN)
#pragma linkage (mlpsetlearn ,FORTRAN)
#pragma linkage (mlpsetnpat ,FORTRAN)
#pragma linkage (mlpsetpat ,FORTRAN)
#pragma linkage (mlpinitw ,FORTRAN)
#pragma linkage (mlpepoch ,FORTRAN)
#pragma linkage (mlpfree ,FORTRAN)
#pragma linkage (mlpprpawf ,FORTRAN)
#pragma linkage (mlpcompute ,FORTRAN)
#endif

/***********************************************************/
/* mlpsetnet                                               */
/*                                                         */
/* to set the structure of a neural network                */
/* inputs:     int *nl = number of layers                  */
/*	       int *nn = number of neurons         	   */
/*                                                         */
/* return value (int) = error value:   		           */
/*                              0: no error		   */ 
/*                              1: N layers > 4		   */ 
/*                              2: N layers < 2		   */ 
/*                              3: N neurons > NMAX        */ 
/*                                                         */
/* Author: J.Schwindling   14-Apr-99                       */
/* Modified: J.Schwindling 05-Oct-99 call MLP_SetNet       */
/***********************************************************/   
                                                
int mlp_setnet(int nl, int *nn)
{
        return MLP_SetNet(&nl,nn);
}

int type_of_call mlpsetnet(nl, nn)
int *nl, *nn;
{
   return mlp_setnet(*nl, nn);
}

/***********************************************************/
/* mlplearnalloc                                           */
/*                                                         */
/* to allocate memory for learning                         */
/* should be called after mlpsetnet                        */   
/*                                                         */
/* Author: J.Schwindling   14-Apr-99                       */
/* Modified: J.Schwindling 31-Jan-00 return value          */
/***********************************************************/   
                                                
int mlplearnalloc_()
{
	return LearnAlloc();
}       	  

      
/***********************************************************/
/* mlplearnfree                                            */
/*                                                         */
/* to free memory for learning                             */
/*                                                         */
/* Author: J.Schwindling   14-Apr-99                       */
/***********************************************************/   
                                                
void mlplearnfree_()
{
	LearnFree();
}       	  

      
/***********************************************************/
/* mlpsetlearn                                             */
/*                                                         */
/* to define learning method and learning parameters       */   
/*                                                         */
/* inputs:     int *lmet = learning method:                */
/*                       1 = stochastic minimozation       */      
/*                       2 = steepest descent fixed steps  */
/*                       3 = steepest descent with L.search*/
/*                       4 = Ribiere-Polak Conjugate Grad. */
/*                       5 = Fletcher-Reeves Conj. Grad.   */
/*                       6 = BFGS                          */
/*                       7 = Hybrid method                 */
/*             float *eta = learning parameter             */
/*                           (used by methods 1 and 2)     */
/*             float *decay = decrease factor (per epoch)  */
/*                            of learning parameter        */
/*             float *epsilon = momentum term (meth. 1,2)  */
/*             float *Delta = flat spot elimination param. */
/*                            (methods 1 or 2)             */
/*             int *nreset = frequency to reset to         */
/*                           steepest gradient (meths 4->7)*/
/*             float *tau = governs precision of line      */
/*                                      search (meths 4->7)*/
/*             float *lambda = size of regularisation term */
/*                             (method 7)                  */                       
/*                                                         */
/* return value (int) = error value:   		           */
/*                              0: no error		   */ 
/*                              1: method > 7 		   */ 
/*                                                         */
/* Author: J.Schwindling   14-Apr-99                       */
/***********************************************************/   
                                                
int mlp_setlearn(int lmet, float eta, float decay, 
                 float epsilon, float Delta, 
                 int nreset, float tau, float lambda)
{
    	int il,in;
	
	if(lmet>7) return(1);
	
	LEARN.Meth = lmet;
	LEARN.Nreset = nreset;
	LEARN.Tau = (dbl) tau;
	LEARN.Decay = (dbl) decay;
	LEARN.Lambda = (dbl) lambda;
        LEARN.eta = (dbl) eta;
        LEARN.delta = (dbl) Delta;
        LEARN.epsilon = (dbl) epsilon;
	
	return LearnAlloc();
}

int type_of_call mlpsetlearn(lmet, eta, decay, epsilon, Delta, nreset, 
                             tau, lambda)
int *lmet;
float *eta;
float *decay;
float *epsilon;
float *Delta;
int *nreset; 
float *tau;
float *lambda;
{
   return mlp_setlearn(*lmet, *eta, *decay, *epsilon, *Delta, 
                       *nreset, *tau, *lambda);
}

/***********************************************************/
/* mlpsetnpat                                              */
/*                                                         */
/* to set the number of examples                           */
/* inputs:     int *ifile = 0: learning examples           */
/*                          1: test examples               */
/*	       int *npat = number of examples         	   */
/*             int *ipond = 0: no weighting                */
/*                          1: weighting                   */
/*                                                         */
/* return value (int) = error value:   		           */
/*                              0: no error		   */ 
/*                                                         */
/* Author: J.Schwindling   14-Nov-98                       */
/* Modified: J.Schwindling 27-Jan-00 nin, nout             */      
/***********************************************************/   

int mlp_setnpat(int ifile, int npat, int ipond, 
                           int nin, int nout)
{
        int ierr;       
	PAT.Npat[ifile] = npat;
	PAT.Iponde = ipond;
        PAT.Nin = nin;
        PAT.Nout = nout;
	ierr = AllocPatterns(ifile,npat,nin,nout,0);

    	return(ierr); 
}

int type_of_call mlpsetnpat(ifile, npat, ipond, nin, nout)
int *ifile, *npat, *ipond, *nin, *nout;
{
   return mlp_setnpat(*ifile, *npat, *ipond, *nin, *nout);
}

/***********************************************************/
/* mlppatnum                                            */
/*                                                         */
/* to set the number of examples                           */
/* inputs:     int *ifile = 0: learning examples           */
/*                          1: test examples               */
/*	       int *npat = number of examples         	   */
/*                                                         */
/* return value (int) = error value:   		           */
/*                              0: no error		   */ 
/*                                                         */
/* Author: J.Schwindling   26-APR-99                       */
/***********************************************************/   

int mlppatnum_ (int *ifile, int *npat)
{
	       
	PAT.Npat[*ifile] = *npat;

    	return(0); 
}


/***********************************************************/
/* mlpaddnpat                                              */
/*                                                         */
/* to add to the examples                                  */
/* inputs:     int *ifile = 0: learning examples           */
/*                          1: test examples               */
/*	       int *npat = number of examples         	   */
/*             int *ipond = 0: no weighting                */
/*                          1: weighting                   */
/*                                                         */
/* return value (int) = error value:   		           */
/*                              0: no error		   */ 
/*                                                         */
/* Author: J.Schwindling   14-Nov-98                       */
/***********************************************************/   

int mlpaddnpat_ (int *ifile, int *npat, int *ipond, int *nin, 
    int *nout)
{
	       
	PAT.Iponde = *ipond;
	PAT.Nin = *nin;
	PAT.Nout = *nout;
	return AllocPatterns(*ifile,*npat,PAT.Nin,
		PAT.Nout,1);
}


/***********************************************************/
/* mlpgetnpat                                              */
/*                                                         */
/* to get the number of examples, the number of            */ 
/* inputs/outputs, the weighting mode                      */
/* inputs:     int *ifile = 0: learning examples           */
/*                          1: test examples               */
/* outputs:    int *npat = number of examples         	   */
/*	       int *ninp = number of inputs                */
/*             int *nout = number of outputs               */   
/*             int *ipond = 0: no weighting                */
/*                          1: weighting                   */
/*                                                         */
/* Author: J.Schwindling   26-Apr-99                       */
/***********************************************************/   

void mlpgetnpat_(int *ifile, int *npat, int *ninp, int *nout, int *ipond)
{
 	*ninp = PAT.Nin;
	*nout = PAT.Nout;
	*ipond = PAT.Iponde;
       	*npat = PAT.Npat[*ifile];
} 


/***********************************************************/
/* mlpinitw                                                */
/*                                                         */
/* initialize weights to random values                     */
/*                                                         */
/* inputs:     int *mode = 0: random weights               */
/*                         1: call srand(1) before         */ 
/*                                                         */
/* Author: J.Schwindling   14-Nov-98                       */
/* Modified: J.Schwindling 28-Jun-99 srand(1)		   */
/***********************************************************/   

void mlp_initw(int mode)
{
	if(mode == 1) 
		{
#if(defined(CERNLIB_WINNT))
                srand(1);
#else
                srandom(1);
#endif
		LastAlpha = 0;
		}
        InitWeights();
}

void type_of_call mlpinitw(mode)
int *mode;
{
   mlp_initw(*mode);
}

/***********************************************************/
/* mlpprw                                                  */
/*                                                         */
/* print weights on screen                                 */
/*                                                         */
/* Author: J.Schwindling   14-Nov-98                       */
/***********************************************************/   

void mlpprw_()
{
	PrintWeights();
}


/***********************************************************/
/* mlpsaveff                                               */
/*                                                         */
/* writes fortran function in a file                       */
/*                                                         */
/* inputs: char *filename: name of the output file         */
/*                                                         */
/* return value (int) = 0: no error                        */
/*                                                         */
/* Author: J.Schwindling   30-Nov-99                       */
/***********************************************************/   

int mlpsaveff_(char *filename)
{
        return MLP_PrFFun(filename);
}


/***********************************************************/
/* mlpprff                                                 */
/*                                                         */
/* writes fortran function in file mlpfun.f                */
/*                                                         */
/* Author: J.Schwindling   14-Nov-98                       */
/***********************************************************/   

void mlpprff_()
{
	MLP_PrFFun("mlpfun.f");
}


/***********************************************************/
/* mlpsavecf                                               */
/*                                                         */
/* writes C function in a file                             */
/*                                                         */
/* inputs: char *filename: name of the output file         */
/*                                                         */
/* return value (int) = 0: no error                        */
/*                                                         */
/* Author: J.Schwindling   30-Nov-99                       */
/***********************************************************/   

int mlpsavecf_(char *filename)
{
        return MLP_PrCFun(filename);
}


/***********************************************************/
/* mlpversion                                              */
/*                                                         */
/* return value (float) = version of the MLPfit package    */
/*                                                         */
/* Author: J.Schwindling   11-Jun-98                       */
/***********************************************************/   

float mlpversion_()
{
	return MLPfitVersion;
}


/***********************************************************/
/* mlpprfun                                                */
/*                                                         */
/* writes fortran or C function in file mlpfun.f or .c     */
/*                                                         */
/* inputs:     int *lang = 1: fortran                      */
/* 	       		 = 2: C		                   */
/*                                                         */
/* Author: J.Schwindling   21-May-99                       */
/***********************************************************/   

void mlpprfun_(int *lang)
{
	if(*lang==1)
		{
		MLP_PrFFun("mlpfun.f");
		}
	else
		{
		MLP_PrCFun("mlpfun.c");
		}	
}


/***********************************************************/
/* mlploadw                                                */
/*                                                         */
/* load weights from file filename                         */
/*                                                         */
/* Author: J.Schwindling   14-Nov-98                       */
/***********************************************************/   

int mlploadw_(char *filename)
{
  	int idummy;
	return LoadWeights(filename,&idummy);
}


/***********************************************************/
/* mlpsavew                                                */
/*                                                         */
/* save weights from file filename                         */
/*                                                         */
/* Author: J.Schwindling   14-Nov-98                       */
/***********************************************************/   

int mlpsavew_(char *filename)
{
	return SaveWeights(filename,0);
}


/***********************************************************/
/* mlpfree                                                 */
/*                                                         */
/* frees memory of the examples                            */
/*                                                         */
/* Author: J.Schwindling   14-Nov-98                       */
/***********************************************************/   

void mlp_free()
{
	int i;

	if(ExamplesMemory==0) return;
	
	for(i=0; i<2; i++)
		{
		FreePatterns(i);	
		} 
       free(PAT.Rin);
       free(PAT.Rans);
       free(PAT.Pond);
       ExamplesMemory = 0; 
       LearnFree();        
}

void type_of_call mlpfree()     
{
       mlp_free();    
}

                                               

/***********************************************************/
/* mlpsetpat                                               */
/*                                                         */
/* to set an example                                       */
/* inputs:     int *ifile = 0: learning example            */
/*                          1: test example                */
/*	       int *ipat = example number (1 -> N)         */
/*	       float *rin = inputs			   */	
/*	       float *rans = answers			   */	
/*	       float *pond = weight			   */	
/*                                                         */
/* return value (int) = error value:   		           */
/*                              0: no error		   */ 
/*                                                         */
/* Author: J.Schwindling   14-Nov-98                       */
/***********************************************************/   

int mlp_setpat(int ifile, int ipat, float *rin, 
               float *rans, float pond)
{
        int in;
	
	for(in=0; in<PAT.Nin; in++) 
		{ 
                PAT.vRin[ifile][(ipat-1)*(PAT.Nin+1)+in+1] = 
                        (type_pat) rin[in];
                }      
        for(in=0; in<PAT.Nout; in++) 
                { 
                PAT.Rans[ifile][ipat-1][in] = (type_pat) rans[in]; 
                }
        PAT.Pond[ifile][ipat-1] = (type_pat) pond;         
		
    	return(0); 
}

int type_of_call mlpsetpat(ifile ,ipat ,rin ,rans ,pond)
int *ifile, *ipat;
float *rin, *rans, *pond;
{
 return mlp_setpat(*ifile, *ipat, rin, rans, *pond);
}
                                                
                                               
/***********************************************************/
/* mlpepoch                                                */
/*                                                         */
/* one epoch of learning                                   */
/*                                                         */
/* inputs:     int *iepoch = epoch number                  */
/*                                                         */
/* return value (float) = error on learning sample         */
/*                                                         */
/* Author: J.Schwindling   14-Nov-98                       */
/***********************************************************/   

float mlp_epoch(int iepoch)
{
	dbl alpmin; 
	float err;
	int i, ntest;
	
	i = iepoch;
	err = (float) MLP_Epoch(i,&alpmin,&ntest);
	return(err);
}

float type_of_call mlpepoch(iepoch)
int *iepoch;
{
   return mlp_epoch(*iepoch);
}

/***********************************************************/
/* mlptest                                                 */
/*                                                         */
/* test the network on one of the files                    */
/*                                                         */
/* inputs:     int *ifile = 0: learning file               */
/*			  = 1: test file		   */
/*                                                         */
/* return value (float) = error 		           */
/*                                                         */
/* Author: J.Schwindling   14-Nov-98                       */
/***********************************************************/   

float mlptest_ (int *ifile)
{
	int i;
	float err;
	
	i = *ifile;
	err = (float) MLP_Test(i,0);
	return(err);
}


/***********************************************************/
/* mlpcompute                                              */
/*                                                         */
/* compute the MLP output(s) for the given input(s)        */
/*                                                         */
/* input:     float *rin: inputs	                   */
/* output:    float *rout: outputs			   */
/*                                                         */
/* Author: J.Schwindling   14-Nov-98                       */
/***********************************************************/   

void type_of_call mlpcompute(float *rin, float *rout)
{
	type_pat *rrin;
	dbl *rrout;
	int in;
	
        rrin = (type_pat*) malloc(PAT.Nin*sizeof(type_pat));

        for(in=0; in<PAT.Nin; in++)
                {
                rrin[in] = (type_pat) rin[in];
                }
        MLP_Out_T(rrin);
        for(in=0; in<PAT.Nout; in++)
                {
                rout[in] = (float) NET.Outn[NET.Nlayer-1][in];
                }
        free(rrin);
}



/***********************************************************/
/* mlpgetnet                                               */
/*                                                         */
/* gets the network structure		                   */
/*                                                         */
/* Author: J.Schwindling   31-May-99                       */
/***********************************************************/   

void mlpgetnet_(int *nlayer, int *nneur)
{
	int i;
	
	*nlayer = NET.Nlayer;
	for(i=0; i<NET.Nlayer; i++)
		{
		nneur[i] = NET.Nneur[i];
		}
}


/***********************************************************/
/* mlpgetw	                                           */
/*                                                         */
/* gets the weights for the given neuron                   */
/*                                                         */
/* inputs:    int *layer : layer number (1 -> N)           */
/*	      int *neuron: neuron number (1 -> N)          */
/* output:    float *weights: weights			   */
/*							   */
/* return value (int) : error code = 0: no error	   */
/*				     1: wrong layer	   */
/*				     2: wrong neuron	   */
/*                                                         */
/* Author: J.Schwindling   31-May-99                       */
/***********************************************************/   

int mlpgetw_(int *layer, int *neuron, float *weights)
{
	int i;
	
	if(*layer<2) return 1;
	if(*layer>NET.Nlayer) return 1;
	if(*neuron>NET.Nneur[*layer-1]) return 2;
	
	for(i=0; i<=NET.Nneur[*layer-2]; i++)
		{
		weights[i] = NET.Weights[*layer-1][*neuron-1][i];
		}
	return 0;	
}


/***********************************************************/
/* mlpsetw	                                           */
/*                                                         */
/* sets the weights for the given neuron                   */
/*                                                         */
/* inputs:    int *layer : layer number (1 -> N)           */
/*	      int *neuron: neuron number (1 -> N)          */
/* output:    float *weights: weights			   */
/*							   */
/* return value (int) : error code = 0: no error	   */
/*				     1: wrong layer	   */
/*				     2: wrong neuron	   */
/*                                                         */
/* Author: J.Schwindling   16-May-00                       */
/***********************************************************/   

int mlpsetw_(int *layer, int *neuron, float *weights)
{
	int i;
	
	if(*layer<2) return 1;
	if(*layer>NET.Nlayer) return 1;
	if(*neuron>NET.Nneur[*layer-1]) return 2;
	
	for(i=0; i<=NET.Nneur[*layer-2]; i++)
		{
		NET.Weights[*layer-1][*neuron-1][i] = (dbl) weights[i];
		}
	return 0;	
}

/***********************************************************/
/* mlpgetinput                                             */
/*                                                         */
/* gets input for a given file / variable                  */
/*                                                         */
/* input:     int *ifile : 0 = learn file                  */
/*                         1 = test file                   */
/*            int *input : input number (1->N)             */
/* output:    float *rin: input values                     */
/*                                                         */
/* Author: J.Schwindling   04-JAN-2000                     */
/***********************************************************/   

void mlpgetinput_(int *ifile, int *input, float *rin)
{
        int i;
        for(i=0;i<PAT.Npat[*ifile]; i++)
        {
                rin[i] = PAT.Rin[*ifile][i][(*input)-1];
        }
}


/***********************************************************/
/* mlpgetoutput                                            */
/*                                                         */
/* gets output for a given file / output                   */
/*                                                         */
/* input:     int *ifile : 0 = learn file                  */
/*                         1 = test file                   */
/*            int *output : output number       (1->N)     */
/* output:    float *rout: output values                   */
/*                                                         */
/* Author: J.Schwindling   07-JAN-2000                     */
/***********************************************************/   

void mlpgetoutput_(int *ifile, int *output, float *rout)
{
        int i;
        for(i=0;i<PAT.Npat[*ifile]; i++)
        {
                rout[i] = PAT.Rans[*ifile][i][(*output)-1];
        }
}


/***********************************************************/
/* mlpgetdata                                              */
/*                                                         */
/* gets data for a given file / pattern                    */
/*                                                         */
/* input:     int *ifile : 0 = learn file                  */
/*			   1 = test file                   */
/*            int *ipat : example number		   */
/* output:    float *rin: inputs			   */
/* 	      float *rout: outputs from current net 	   */
/*            float *rans: desired answers		   */
/*	      float *pond: event weight			   */
/*                                                         */
/* Author: J.Schwindling   26-APR-99                       */
/***********************************************************/   

void mlpgetdata_(int *ifile, int *ipat, float *rin, float *rout, float *rans,
		 float *pond)
{
	type_pat *rrin;
	dbl *rrout;
	int in;
	
	rrin = (type_pat*) malloc(NET.Nneur[0]*sizeof(type_pat));
	rrout = (dbl*) malloc(NET.Nneur[NET.Nlayer-1]*sizeof(dbl));
	
	for(in=0; in<NET.Nneur[0]; in++)
		{
		rrin[in] = PAT.Rin[*ifile][*ipat-1][in];
		rin[in] = (float) rrin[in];
		}
	MLP_Out(rrin,rrout);
	for(in=0; in<NET.Nneur[NET.Nlayer-1]; in++)
		{
		rans[in] = (float) PAT.Rans[*ifile][*ipat-1][in];
		rout[in] = (float) rrout[in];
		}
	*pond = (float) PAT.Pond[*ifile][*ipat-1];
	free(rrin);
	free(rrout);
}


/***********************************************************/
/* mlpprpawf                                               */
/*                                                         */
/* prints out a function which can be used in PAW          */
/*                                                         */
/* Author: J.Schwindling   26-Apr-99                       */
/***********************************************************/   

void type_of_call mlpprpawf()
{
	int il,in,jn;
	FILE *W;

	W=fopen("pawmlp.f","w");
	
	if(NET.Nneur[0]==1) fprintf(W,"      REAL FUNCTION PAWMLP(X)\n");
	if(NET.Nneur[0]==2) fprintf(W,"      REAL FUNCTION PAWMLP(X,Y)\n");
	fprintf(W,"C\n");
	
	fprintf(W,"      OUT1 = X\n");
	if(NET.Nneur[0]==2) fprintf(W,"      OUT2 = Y\n");
	
	for(il=1; il<NET.Nlayer-1; il++)
		{
		fprintf(W,"C\n");
		fprintf(W,"C     layer %d\n",il+1);
		for(in=0; in<NET.Nneur[il]; in++)
			{
			fprintf(W,"      RIN%d = %lf\n",in+1,
					(double) NET.Weights[il][in][0]);
			for(jn=1;jn<=NET.Nneur[il-1]; jn++)
				fprintf(W,"     > +(%lf) * OUT%d\n",
					(double) NET.Weights[il][in][jn],jn);
			}
		fprintf(W,"C\n");
		for(in=0; in<NET.Nneur[il]; in++)
			{
			fprintf(W,"      OUT%d = SIGMOID(RIN%d)\n",
				in+1,in+1);
			}
		}
	il = NET.Nlayer-1;
	fprintf(W,"C\n");
	fprintf(W,"C     layer %d\n",il+1);
	fprintf(W,"      PAWMLP = %lf\n",
			(double) NET.Weights[il][0][0]);
	for(jn=1;jn<=NET.Nneur[il-1]; jn++)
		fprintf(W,"     > +(%lf) * OUT%d\n",
			(double) NET.Weights[il][0][jn],jn);
		
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


/***********************************************************/
/* mlpreadf                                                */
/*                                                         */
/* to read learn.pat or test.pat file                      */
/*                                                         */
/* Author: J.Schwindling   26-Apr-99                       */
/* Modified: J.S. 21-Sep-99  add filename and return value */ 
/* Modified: J.S. 22-Dec-1999 FreePatterns                 */
/***********************************************************/   

int mlpreadf_(int *ifile, char *filename)
{
        int idummy1, idummy2, idummy3;
        if(PAT.Npat[*ifile] != 0)
        {
                FreePatterns(*ifile);
        }
        return ReadPatterns(filename, *ifile, &idummy1, &idummy2, &idummy3);
}


/***********************************************************/
/* mlpsettf                                                */
/*                                                         */
/* to set the transfer function of a given neuron          */
/*                                                         */
/* inputs:  int *layer: layer number (2 -> Nlayer)	   */
/*          int *neuron: neuron number (1 -> Nneur)        */
/*          int *tfunc = 0: neuron not activated           */
/*		       = 1: linear neuron		   */
/*		       = 2: sigmoid neuron		   */   
/*                                                         */
/* return code (int)    = 0: no error			   */
/*			= 1: wrong layer number		   */
/*			= 2: wrong neuron number	   */
/*			= 3: wrong transfer function	   */
/*                                                         */
/* Author: J.Schwindling   07-May-99                       */
/***********************************************************/   

int mlpsettf_(int *layer, int *neuron, int *tfunc)
{
	if(*layer<2 || *layer>NET.Nlayer) return 1;
	if(*neuron<1 || *neuron>NET.Nneur[*layer-1]) return 2;
	if(*tfunc<0 || *tfunc>2) return 3;
	
	SetTransFunc(*layer,*neuron,*tfunc);
	return 0;
}


/***********************************************************/
/* mlpgettf                                                */
/*                                                         */
/* to get the transfer function of a given neuron          */
/*                                                         */
/* inputs:  int *layer: layer number (2 -> Nlayer)	   */
/*          int *neuron: neuron number (1 -> Nneur)        */
/* output   int *tfunc = 0: neuron not activated           */
/*		       = 1: linear neuron		   */
/*		       = 2: sigmoid neuron		   */   
/*                                                         */
/* return code (int)    = 0: no error			   */
/*			= 1: wrong layer number		   */
/*			= 2: wrong neuron number	   */
/*			= 3: wrong transfer function	   */
/*                                                         */
/* Author: J.Schwindling   07-May-99                       */
/***********************************************************/   

int mlpgettf_(int *layer, int *neuron, int *tfunc)
{
	if(*layer<2 || *layer>NET.Nlayer) return 1;
	if(*neuron<1 || *neuron>NET.Nneur[*layer-1]) return 2;
	*tfunc = NET.T_func[*neuron-1][*layer-1];
	return 0;
}


/***********************************************************/
/* mlpnorm                                                 */
/*                                                         */
/* to normalize the inputs to mean = 0, rms = 1            */
/*                                                         */
/* Author: J.Schwindling   18-May-99                       */
/***********************************************************/   

void mlpnorm_()
{
	DIVERS.Norm = 1;
	NormalizeInputs();	
}


/***********************************************************/
/* mlpstat                                                 */
/*                                                         */
/* returns statistics                                      */
/*                                                         */
/* inputs: ifile (int *): file number                      */
/* outputs: mean (float *):     mean values                */
/*          sigma (float *):  sigmas                       */
/*                      minimum (float *): minimum values  */        
/*                      maximum (float *): maximum values  */        
/*                                                         */
/* Author: J.Schwindling   21-Dec-1999                     */
/***********************************************************/   

void mlpstat_(int *ifile, float *mean, float *sigma,
                                        float *minimum, float *maximum)
{
        dbl *Mean, *Sigma, *Minimum, *Maximum;
        int Nexamples, Ninputs, i;

        Nexamples = PAT.Npat[*ifile];
        Ninputs = PAT.Nin;

        Mean = (dbl *) malloc(Ninputs * sizeof(dbl));
        Sigma = (dbl *) malloc(Ninputs *sizeof(dbl));
        Minimum = (dbl *) malloc(Ninputs * sizeof(dbl));
        Maximum = (dbl *) malloc(Ninputs *sizeof(dbl));
	
        MLP_StatInputs(Nexamples, Ninputs, PAT.Rin[*ifile], 
                Mean, Sigma, Minimum, Maximum);

        for(i=0; i<Ninputs; i++)
        {
                mean[i] = (float) Mean[i];
                sigma[i] = (float) Sigma[i];
                minimum[i] = (float) Minimum[i];
                maximum[i] = (float) Maximum[i];
        }

        free(Mean);
        free(Sigma);
        free(Minimum);
        free(Maximum);
}
