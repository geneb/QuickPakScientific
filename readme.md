On October 26th, 2018 I purchased the software assets of Full Moon Software.
Full Moon Software used to be known as Crescent Software.  They produced a line
of excellent development libraries for MS-DOS.  The supported environments were
QuickBASIC 4.x, Microsoft Professional Development System v7.x, and Visual 
Basic for DOS.

The idea behind obtaining these products was to release them to the public
domain to ensure that people could still access these things in the future.
While most developers will have no use for these products in a modern 
develoment environment, they still have value as an example of "how it was 
done" back in the heyday of x86 DOS development. 

The software in this repository hasn't been modified from how I received it 
from Ethan Winer, the original author.  While all the source files carry some 
kind of Copyright notice, the software is now in the public domain.

The contents of the installation floppies will be uploaded to the Internet
Archive soon and when the manuals are scanned, they'll be uploaded there
as well.  I'll update this readme file with a link to the manual scan when
it's available.

The original distribution disk files are available here:

http://annex.retroarchive.org/crescent/QPSCI.ZIP


Gene Buckle, October 27th, 2018
-------------------------------------------------------------------------------
About Crescent Software:
After 20 years as a professional recording engineer and musician, Ethan
Winer founded Crescent Software in 1986, quickly building it to become the
leading provider of add-on products for use with Microsoft compiled BASIC
for DOS. During that time Ethan wrote numerous articles about DOS BASIC and
assembly language for all of the major programming magazines, and also
served as a contributing editor for PC Magazine. Ethan also received
Microsoft's MVP award every year since 1996 for his assistance in the
Microsoft BASIC programming newsgroups. In 1992 Ethan sold Crescent to his
partner Don Malin, and retired in order to pursue his musical interests.
===============================================================================

I've attached the text from Full Moon Software's catalog description of 
QuickPak Scientific below.

-------------------------------------------------------------------------------
QUICKPAK(tm) SCIENTIFIC
=======================

A Powerful Numerical Analysis Toolbox for DOS Compiled BASIC
------------------------------------------------------------

QuickPak Scientific is a comprehensive collection of subroutines and functions 
that add numerical analysis to your BASIC programs. Each routine is provided 
in the form of a flexible and easy to use BASIC algorithm designed to help you 
solve practical and challenging problems in engineering, science, and other 
technical applications. QuickPak Scientific consists of three key components 
designed to simplify your numerical analysis programming:

     * Source code for state-of-the-art BASIC subroutines and functions. These
       routines perform a variety of fundamental and sophisticated numerical
       analysis tasks which may be too time-consuming or difficult to program
       and debug yourself.

     * Interactive BASIC demonstration programs. These sample programs
       illustrate the proper procedure for working with each QuickPak
       Scientific routine. Many of these programs also provide an example
       problem for solution, and each program displays information about the
       algorithm and its performance.

     * The QuickPak Scientific owner's manual. This comprehensive document
       describes the procedures for integrating the QuickPak Scientific
       algorithms into your own BASIC applications. It also serves as a
       tutorial about the art and science of numerical analysis and includes
       many programming hints and tips.

Every QuickPak Scientific algorithm includes a complete description of the 
required input and resultant output. Many of the demonstration programs offer 
an example problem for solution--the software interactively prompts you for 
the required inputs, and several programs also recommend typical input values. 
Other interactive programs allow the BASIC programmer to assess the effects of 
such things as step size, algorithm iterations, convergence criteria, and 
other factors on the behavior and performance of a particular numerical 
method. To illustrate this point, the following is a typical output from the 
QuickPak Scientific Simpson Integration program:

     +----------------------------------------------------------------+
     |                        PROGRAM DEMOINT5.BAS                    |
     |           Adaptive Integration of User-Defined Functions       |
     |    Function Lower Integration Limit = 0                        |
     |    Upper Integration Limit          = 1                        |
     |    Solution Accuracy                = .00000001                |
     |    Integral Value                   = .74682413                |
     |    Estimated Error                  =  1.0925039D-10           |
     +----------------------------------------------------------------+

     * SYSTEMS OF LINEAR EQUATIONS: The Linear Algebra routines solve systems 
of linear equations using the LU decomposition, Gauss-Jordan elimination, and 
iterative improvement methods. An algorithm is also included for solving 
tridiagonal systems of linear equations using Gaussian elimination with 
partial solving.

     * ORDINARY AND PARTIAL DIFFERENTIAL EQUATIONS: QuickPak Scientific 
contains a complete set of algorithms for solving first- and second-order 
systems of ordinary differential equations. These methods include the classic 
fourth-order Runge-Kutta and Nystrom methods, three adaptive Runge-Kutta-
Fehlberg algorithms, and a variable-order Adams-Bashforth-Moulton predicator-
corrector subroutine. Efficient BASIC subroutines are also provided for 
solving the Poisson heat and wave partial differential equations.

     * INTEGRATION AND DIFFERENTIATION: The ability to numerically integrate 
user-defined analytic and tabulated functions is also part of QuickPak 
Scientific. Tabular data can be integrated with either Simpson or cubic spline 
subroutines. Single definite integrals of analytic user-defined functions can 
be quickly and accurately evaluated with a Romberg algorithm. A Composite 
Simpson method is also provided for integrating both double and triple 
definite integrals of analytic functions you define. An adaptive integration 
method based on Simpson's method completes this series of algorithms. The 
derivatives of both user-defined analytic and tabulated functions can also be 
calculated with QuickPak Scientific. These three differentiation subroutines 
perform numerical differentiation by the classic methods of finite-divided 
differences, Lagrange's method, and cubic splines.

     * NON-LINEAR EQUATIONS AND OPTIMIZATION: QuickPak Scientific has six 
subroutines for non-linear equations. These solve for the real roots of the 
general, quadratic, cubic, and quartic equations. This series of routines also 
includes an algorithm for computing the real and complex roots of any 
polynomial up to order 36. Two other routines solve single non-linear 
equations, both with and without derivatives. Non-linear optimization is the 
most powerful numerical method for technical applications--it's also the most 
difficult to implement. QuickPak Scientific provides five flexible and 
powerful algorithms for this purpose. These subroutines can be used to solve 
for the minimum or maximum of scalar functions of one or more variables. A 
complete program is included for solving the constrained, non-linear 
optimization problem. (The source is more than 20,000 lines of BASIC code!)

     * INTERPOLATION AND CURVE FITTING: Important technical information often 
exists in the form of empirical or experimental data which must be carefully 
interpreted. To address this need, QuickPak Scientific includes several 
routines for interpolating tabulated data in the form y = f(x) using both 
natural and clamped cubic spline techniques. A subroutine is also provided 
which can linearly interpolate both two- and three-dimensional tabular data. 
Curve-fitting of experimental data is very important in science and 
engineering, and QuickPak Scientific provides three flexible subroutines for 
this purpose. The first algorithm can fit data to simple linear, log, and 
exponential functions. The second performs a least-squares fit to data of the 
form y = f(x), and the third subroutine calculates fitting coefficients to a 
three-dimensional surface of the form z = f(x, y) using a Maclaurin series.

     * FAST FOURIER TRANSFORMS: QuickPak Scientific offers two algorithms 
which compute the forward and inverse Fast Fourier transforms of real or 
complex data. These routines transform both one- and two-dimensional data 
using the Danielson-Lanczos or bit reversal methods.

     * STATISTICS AND COMPLEX NUMBERS: Also included are routines for 
computing characteristics of Normal, Chi-squared, F-distribution, and T-
distribution statistical functions. These algorithms can determine point x 
from probability and degrees of freedom, or determine probability for point x 
for each type of statistical distribution. QuickPak Scientific includes eight 
routines for performing calculations with complex numbers. These subroutines 
can add, subtract, multiply, and divide two complex numbers, raise a complex 
number to a power, compute its Nth and square root, and find the reciprocal of 
a complex number.

     * TRIGONOMETRY: BASIC's built-in capabilities are extended with the 
QuickPak Scientific trigonometry routines. These flexible functions provide 
easy-to-use inverse sine, cosine, tangent, and hyperbolic functions.

     * VECTORS AND MATRICES: The QuickPak Scientific matrix subprograms 
provide flexible algorithms for performing a variety of calculations involving 
matrices. Included are subroutines for computing the inverse and determinant 
of a square matrix, eigenvalues and eigenvectors, rank of a matrix, and 
fundamental matrix operations, such as addition, subtraction, and 
multiplication. The vector routines let you easily perform numerical 
calculations involving vectors. These calculations include the dot and cross 
product of two vectors; fundamental operations such as vector addition, 
subtraction, and multiplication; and the triple scalar and vector products.

THE FULL MOON PHILOSOPHY

As with all our products, full source code is provided at no additional cost, 
so you can see how the routines were designed and even modify them if you 
want. We genuinely want you to understand how our libraries work and be able 
to learn from them. All of our products are reasonably priced and include free 
technical assistance, but they are licensed for use by only one person using 
one computer at a time. Royalty payments are not required when our routines 
are incorporated into your compiled applications. However, you may not 
distribute our source, object, or library files. If your customers need to 
rebuild your program, they will need their own copy of our product(s).

THE BOTTOM LINE

QuickPak Scientific costs $149 and works with QuickBASIC 4.x, PDS 7.x, and 
VB/DOS. Add $8 for UPS ground shipping to US addresses only (no P.O. boxes); 
Connecticut residents must add 6.0% sales tax or show proof of tax-exempt 
status when ordering. Please call for overnight and foreign shipping costs. We 
accept checks, MasterCard, and VISA. We do accept purchase orders, but they 
must be accompanied by full payment.

QuickPak(tm) Scientific is a trademark of Crescent Software, Inc.