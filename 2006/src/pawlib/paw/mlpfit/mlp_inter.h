int 	mlp_setnet 	(int nl, int *nn);
int 	mlplearnalloc_	();
void 	mlplearnfree_	();
int 	mlp_setlearn	(int lmet, float eta, float decay, 
			float epsilon, float Delta, 
              		int nreset, float tau, float lambda);
int 	mlp_setnpat 	(int ifile, int npat,
int ipond, 		int nin, int nout);
int 	mlppatnum_ 	(int *ifile, int *npat);
int 	mlpaddnpat_ 	(int *ifile, int *npat, int *ipond, int*nin,
			int *nout);
void 	mlpgetnpat_	(int *ifile, int *npat, int *ninp, int *nout, 
			int *ipond);
void 	mlp_initw	(int mode);
void 	mlpprw_		();
void 	mlpprff_	();
void	mlpprfun_	(int *lang);
int 	mlpsaveff_	(char *filename);
int 	mlpsavecf_	(char *filename);
void 	mlploadw_	(char *filename);
void 	mlpsavew_	(char *filename);
void 	mlp_free	();
int 	mlp_setpat 	(int ifile, int ipat, float *rin, float *rans, 
			float pond);
float 	mlp_epoch  	(int iepoch);
float 	mlptest_ 	(int *ifile);
void 	mlpcompute_	(float *rin, float *rout);
void 	mlpgetdata_	(int *ifile, int *ipat, float *rin, float *rout, 
			float *rans, float *pond);
/*void 	mlpprpawf_	();*/
int 	mlpreadf_	(int *ifile, char *filename);
void 	mlpnorm_	();

void 	mlpgetnet_(int *nlayer, int *nneur);
int 	mlpgetw_(int *layer, int *neuron, float *weights);
int 	mlpsetw_(int *layer, int *neuron, float *weights);

float	mlpversion_	();

void 	mlpstat_	(int *ifile, float *mean, float *sigma,
                        float *minimum, float *maximum);
void 	mlpgetinput_	(int *ifile, int *input, float *rin);
void 	mlpgetoutput_	(int *ifile, int *input, float *rin);
