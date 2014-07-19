#ifndef MLPHEADER
#define MLPHEADER

#define NPMAX 100000
#define NLMAX 1000

typedef double dbl;
typedef float type_pat;

/* definition du reseau */
struct 
{
  int Nlayer, *Nneur, Nweights;
  dbl ***Weights;
  dbl **vWeights;
  dbl **Deriv1, **Inn, **Outn, **Delta;
  int **T_func;
  int Rdwt, Debug;
} net_;
#define NET net_   

/* apprentissage */
struct
{
	int Nepoch, Meth, Nreset;
	dbl Tau,Norm,Decay,Lambda,Alambda;
	dbl eta, epsilon, delta;
	dbl ***Odw;
	dbl ***DeDw, ***ODeDw;
	dbl **vDeDw;
} learn_;
#define LEARN learn_

struct 
{
	int Npat[2],Iponde, Nin, Nout;
	type_pat ***Rin, ***Rans, **Pond;
	type_pat **vRin; 
	dbl Ponds[10];
} pat_;
#define PAT pat_

struct
{
	int Dbin;
	int Ihess;
	int Norm, Stat;
	char Outf;
} divers_;
#define DIVERS divers_	

struct
{
	dbl *mean,*sigma;
} mlpstatstruct_;
#define STAT mlpstatstruct_ 	

#if (defined(EXTERNMLP_GEN))
int MessLang = 0;
int OutputWeights = 100;
int ExamplesMemory = 0;
int WeightsMemory = 0;
int PatMemory[2] = {0,0};
int BFGSMemory = 0;
int JacobianMemory = 0;
int LearnMemory = 0;
int NetMemory = 0;
float MLPfitVersion = (float) 1.40;
dbl LastAlpha = 0;
int NLineSearchFail = 0;
#else
extern int MessLang;
extern int OutputWeights;
extern int ExamplesMemory;
extern int WeightsMemory;
extern int PatMemory[2];
extern int BFGSMemory;
extern int JacobianMemory;
extern int LearnMemory;
extern int NetMemory;
extern float MLPfitVersion;
extern dbl LastAlpha;
extern int NLineSearchFail;
#endif

dbl ***dir;
dbl *delta;
dbl **BFGSH;
dbl *Gamma;
dbl **JacobianMatrix;
int *ExamplesIndex;
dbl **Hessian;

void 	MLP_Out(type_pat *rrin, dbl *rrout);
void 	MLP_Out2(type_pat *rrin);
void 	MLP_Out_T(type_pat *rrin);
dbl  	MLP_Test(int ifile, int regul);
dbl 	MLP_Epoch(int iepoch, dbl *alpmin, int *ntest);
int 	MLP_Train(int *ipat,dbl *err);
dbl 	MLP_Stochastic();

int 	StochStep();

dbl 	DeDwNorm();
dbl 	DeDwProd();
void 	DeDwZero();
void 	DeDwSaveZero();
void 	DeDwScale(int Nexamples);
void 	DeDwSave();
int 	DeDwSum(type_pat *ans, dbl *out, int ipat);

int 	SetTransFunc(int layer, int neuron, int func);
void 	SetDefaultFuncs();

void 	SteepestDir();
void 	CGDir(dbl beta);
dbl 	DerivDir();
void 	GetGammaDelta();
void 	BFGSdir(int Nweights);
void 	InitBFGSH(int Nweights);
int 	GetBFGSH(int Nweights);

int 	LineSearch(dbl *alpmin, int *Ntest, dbl Err0);
int 	DecreaseSearch(dbl *alpmin, int *Ntest, dbl Err0);
void  	MLP_ResLin();
void 	MLP_Line(dbl ***w0, dbl alpha);
int		LineSearchHyb(dbl *alpmin, int *Ntest);
void  	MLP_LineHyb(dbl ***w0, dbl alpha);
int 	StochStepHyb();
int		FixedStep(dbl alpha);

void    EtaDecay();
int 	ShuffleExamples(int n, int *index);
double 	MLP_Rand(dbl min, dbl max);
void	InitWeights();
int	NormalizeInputs();
int 	MLP_StatInputs(int Nexamples, int Ninputs, type_pat **inputs, 
		dbl *mean, dbl *sigma, dbl *minimum, dbl *maximum);	
int 	MLP_PrintInputStat();

int 	LoadWeights(char *filename, int *iepoch);
int 	SaveWeights(char *filename, int iepoch);

void 	SetLambda(double Wmax);
void 	PrintWeights();
int		ReadPatterns(char *filename, int ifile, 
		     int *inet, int *ilearn, int *iexamples);
int 	CountLexemes(char *string);
void 	getnLexemes(int n, char *s, char **ss);
void 	getLexemes(char *s,char **ss);
int     LearnAlloc();
void 	LearnFree();
int	MLP_PrFFun(char *filename);
int	MLP_PrCFun(char *filename);
int 	AllocPatterns(int ifile, int npat, int nin, int nout, int iadd);
int 	FreePatterns(int ifile);
void 	AllocWeights();
void	FreeWeights();
int 	AllocNetwork(int Nlayer, int *Neurons);
void	FreeNetwork();
int 	GetNetStructure(char *s, int *Nlayer, int *Nneur);
int 	MLP_SetNet(int *nl, int *nn);
void 	MLP_MM2rows(dbl* c, type_pat* a, dbl* b,
             int Ni, int Nj, int Nk, int NaOffs, int NbOffs);
void 	MLP_MatrixVector(dbl *M, type_pat *v, dbl *r, int n, int m);
void 	MLP_MatrixVectorBias(dbl *M, dbl *v, dbl *r, int n, int m);

#endif

