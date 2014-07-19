*
* $Id: mlpdef.cdf,v 1.5 2001/05/21 09:12:35 couet Exp $
*
* $Log: mlpdef.cdf,v $
* Revision 1.5  2001/05/21 09:12:35  couet
* - new option in MLP/LEARN:
*     B  Keep the weights from the smallest error on the test sample
*
* Revision 1.4  2001/04/03 09:24:29  couet
* - update the help of MANY_PLOTS
* - run ispell on the cdf files
* - revise the keywords in pawcdf.cdf
*
* Revision 1.3  2000/11/07 11:06:37  couet
* ^ is replaced by ** on the MLP/LPAT/SET help. It was producing bad LaTex
* reference manual
*
* Revision 1.2  1999/09/17 07:32:31  couet
* - New version of the mlpfit commands
*
* Revision 1.1  1999/08/31 08:47:07  couet
* - A new set of PAW commands using Neural Networks.
*   These new commands are using the MLPfit package.
*
*
>Name MLPDEF

>Menu MLP
>Keyword 
Neural Network Multilayer Perceptron 
>Guidance 
Multi-Layer Perceptron (MLP).
.
Neural Networks in general, and in particular the Multi-Layer Perceptron (MLP)
are now very widely used in several fields, for example:
. 
 - in industry for automatic process control, quality control, 
   optimisation of resources allocation.
 - for medical images analysis and help to diagnosis.
 - in meteorology for weather forecast.
 - ...
.
In Particle Physics, they are commonly used, mainly for off-line classification
tasks (particle identification, event classification, search for new physics).
They are also used for track reconstruction or for online triggering.
.
The Multi-layer perceptron is the most widely used type of neural network. It is
both simple and based on solid mathematical grounds. Input quantities are
processed through successive layers of "neurons". There is always an input
layer, with a number of neurons equal to the number of variables of the problem,
and an output layer, where the Perceptron response is made available, with a
number of neurons equal to the desired number of quantities computed from the
inputs (very often only one). The layers in between are called "hidden" layers.
With no hidden layer, the perceptron can only perform linear tasks (for example
a linear discriminant analysis, which is already useful). All problems which can
be solved by a Perceptron can be solved with only hidden layer, but it is
sometimes more efficient to use 2 hidden layers. Each neuron of a layer other
than the input layer computes first a linear combination of the outputs of the
neurons of the previous layer, plus a bias. The coefficients of the linear
combinations plus the biases are called the weights. They are usually determined
from examples to minimise, on the set of examples, the (Euclidian) norm of the
desired output - net output vector.Neurons in the hidden layer then compute a
non-linear function of their input. In MLPfit, the non-linear function is the
sigmoid function y(x) = 1/(1+exp(-x))). The output neuron(s) has its output
equal to the linear combination. Thus, a Multi-Layer Perceptron with 1 hidden
layer basically performs a linear combination of sigmoid function of the inputs.
A linear combination of sigmoids is useful because of the two following 
theorems:
.
 - a linear function of sigmoids can approximate any continuous function 
   of one or more variable(s). This is useful to obtain a continuous 
   function fitting a finite set of points when no underlying model is 
   available.
 - trained with a desired answer = 1 for signal and 0 for background, 
   the approximated function is the probability of signal knowing the 
   input values. This second theorem is the basic ground for all 
   classification applications.
. 
The Multi-Layer perceptron interface in PAW:
 - can be used for both approximation and classification tasks.
 - provides efficient minimisation methods to determine the weights.
 - allows to interactively define, train and use the neural network.
.
More precisely, it is possible to:
 - define the network structure
 - modify the default learning parameters
 - read/write weight files
 - define the examples from ASCII files, histograms or Ntuples. 
   When examples are defined from Ntuples, selection criteria may 
   be added
 - train the network and follow the learning curve while training
 - plot the Perceptron function in case of 1d or 2d fits, write out the 
   function for use in any other code.

>Command CREATE
>Parameters
NIN 'Number of neurons in the input layer' I R=1:100
+
NHID1 'Number of neurons in the first hidden layer' I D=10 R=0:100
NHID2 'Number of neurons in the second hidden layer' I D=0 R=0:100
NOUT 'Number of neurons in the output layer' I D=1 R=1:100
>Guidance 
Creates a Neural Network. 
.
Example:
.
 PAW > mlp/create 2 4   
.
creates a neural network with 2 inputs, 4 hidden neurons and one output. 
.
 PAW > mlp/create 2 4 ! 2
.
creates a neural network with 2 inputs, 4 hidden neurons and two outputs.  
>Action mlppawc%C

>Command STATUS
>Guidance 
Prints the status of MLP package.
.
The parameter printed are: size of the network, learning method
and parameters, number of examples loaded...
>Action mlppawc%C

>Command LMET
>Parameters
LMET    'Learning Method'             I R=1:7
+
PAR1    'First parameter'             R D=-999.
PAR2    'Second parameter'            R D=-999.
PAR3    'Third parameter'             R D=-999.
>Guidance 
Set learning method and parameters.
.
The following methods are available:

 - 1: stochastic minimization (often wrongly called "standard online
                               back propagation")
 - 2: steepest descent with fixed steps ("batch back propagation")
 - 3: steepest descent with line search
 - 4: conjugate gradients with Polak-Ribiere updating formula
 - 5: conjugate gradients with Fletcher-Reeves updating formula  
 - 6: Broyden - Fletcher - Goldfarb - Shanno (BFGS) method
 - 7: Hybrid linear-BFGS method
.
For methods 1 and 2:
 - PAR1 = learning parameter (default 0.1),
 - PAR2 = momentum term (default 0.),
 - PAR3 = decay factor (default 1.). 
.
For methods 3 -> 6:
 - PAR1 = reset frequency (default = 1000 epochs),
 - PAR2 = tau value for line search (default = 1.5)
.
For method 7: in addition to the parameters used by methods 3 -> 6, PAR3 =
regularisation term (default = 1)
.
By default, MLPfit uses the BFGS learning method, which is stable and probably
efficient enough for most applications.
>Action mlppawc%c

>Command RESET
>Guidance
Reset the neural network.
.
Reset everything concerning the neural net to 0, frees memory.
>Action mlppawc%c

>Command LEARN
>Parameters
NEPOCH 'Number of epochs' I
+
CHOPT 'Options' C D=' '
-+   Start from previous weights (by default start from random weights)
-I   Change random weights to start with 
-Q   Quiet mode
-N   No drawing: the learning curve is not displayed
-B   Keep the weights from the smallest error on the test sample 
FILENAME 'Name of the MLP function' C D='pawmlp.f'

>Guidance
Train the Neural Network for NEPOCH epochs. 
.
The learning curve is saved in
histogram 2000000 (which is reset if already existing). If a test file is also
used, the error curve on the test examples is stored in histogram 2000001.
>Action mlppawc%C

>Menu WEIGHTS

>Command WRITE
>Parameters
+
FILENAME	'File name'	C D='weights.dat'
>Guidance
Write weights in file FILENAME
>Action mlppawc%C

>Command READ
>Parameters
+
FILENAME	'File name'	C D='weights.dat'
>Guidance
Read weights from file FILENAME
>Action mlppawc%C
 
>Menu ../LPAT
>Guidance
Operations on the learning patterns
>Command READ
>Parameters
+
FILENAME	'File name'	C D='learn.pat'
>Guidance
Read examples from file FILENAME
>Action mlppawc%C

>Command WRITE
>Parameters
+
FILENAME	'File name'	C D='learn.pat'
>Guidance
write examples to file FILENAME
>Action mlppawc%C

>Command SET
>Parameters
IDN     'Histogram or Ntuple Identifier'        C
+
INLIST  'List of inputs'              C D=' '
OUTLIST 'List of outputs'             C D=' '
WEIGHT  'Weight'                      C D='1.'
NEVENT  'Number of examples'          I D=9999999
IFIRST  'First example'               I D=1
CUTS    'Cuts'			      C D=' '
CHOPT   'Options'                     C D=' '   
-+   add the examples to the already existing ones
>Guidance
Set learning examples from an N-tuple or an histogram. 
.
When IDN is a
histogram, INLIST can be 'E' to weights the examples by the (histogram) error
bars. For example:
.
 PAW > mlp/lpat/set 100 E       
.
sets the examples from the 1d or 2d histogram number
100. The number of examples is equal to the number of bins. The inputs are the
abscissa values, the desired answers are set the value of the corresponding
channel. Because of the 'E' option, each example is weighted by 1/e**2, where e
is the error in the corresponding bin. If the option 'E' is not used, the
example weights are all equal to 1. 
. 
 PAW > mlp/lpat/set 1000 var1%sqrt(var2)%var3+var4 var5 1. 1000 1 var6>0.      
.
sets the examples from Ntuple 1000. The 3 input quantities are var1, sqrt(var2)
and var3+var4. The desired answer is var5. The examples are not weighted 
(weights all equal to 1). 1000 examples are considered, starting at number 1. 
Only the examples with var6>0 are indeed used. If the option '+' is
given, the examples are added to the already existing ones.
>Action mlppawc%c

>Menu ../TPAT
Operations on the test patterns
.
Whereas it is obviously necessary to define learning
examples before training the network, the test examples are optional (but
encouraged).  

>Command READ
>Parameters
+
FILENAME	'File name'	C D='learn.pat'
>Guidance
Read examples from file FILENAME
>Action mlppawc%C

>Command WRITE
>Parameters
+
FILENAME	'File name'	C D='learn.pat'
>Guidance
write examples to file FILENAME
>Action mlppawc%C

>Command SET
>Guidance
To set test examples from an N-tuple or an histogram. See the command
MLP/LPAT/SET for more details.
>Parameters
IDN     'Histogram or Ntuple Identifier'        C
+
INLIST  'List of inputs'              C D=' '
OUTLIST 'List of outputs'             C D=' '
WEIGHT  'Weight'                      C D='1.'
NEVENT  'Number of examples'          I D=9999999
IFIRST  'First example'               I D=1
CUTS    'Cuts'			      C D=' '
CHOPT   'Options'                     C D=' '   
-+   add the examples to the already existing ones
>Action mlppawc%c
