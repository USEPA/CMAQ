<
[<< Previous Chapter](CMAQ_OGD_ch10_new_simulation) - [Home](CMAQ_OGD_index) - [Next Chapter >>](CMAQ_OGD_ch12_analysis_tools)


CODE MANAGEMENT AND DEVELOPMENT
===============================

As a public domain model, CMAQ is the product of contributions from many developers, whose numbers are only expected to increase with the number of users worldwide. Some degree of standardization is necessary for management and archiving of these development versions, as well as to compile and execute the code once it is ready for use, and to submit it to the CMAS Center for archiving and benchmark testing. This chapter provides guidance on source code manage­ment, coding guidelines for new code development, the compilation of new source code using the build scripts, and guidelines for writing shell scripts usable by CMAQ. Much of this informa­tion is derived from Chapter 18 (Young, 1999) in Byun and Ching (1999), with updates where appropriate, particularly for new versions of the model code and for the Fortran 90 standard. The chapter also includes the procedure that is in place for distributing code versions other than the operational CMAQ that are submitted to the development code archives.

Source Code Management
----------------------

### The need for a configuration-management tool

Faced with a large and growing community that uses and develops a wide variety of programs, modules, and codes, it is imperative to systematically manage the cross-community access to this software. Typically, successful management of software involves the following:

-   A repository – a place where all of the public code resides.
-   The concept of archived code – codes that have been deposited into the repository in such a manner that anyone can extract the exact code at a later time. This involves some kind of transformation program to maintain master copies of the codes with embedded change tables.
-   The concept of revision control – archiving codes results in modifying the tags or unique revision identifiers in the change tables in the master copies in order to recover the exact code at a later date.
-   The concept of released code – codes that have reached some state of maturity and have been designated with some kind of “released” status. They can be used with reasonable expectation of reliability. The paradigm used employs the following scenario:
    1.  A user modifies or develops code. The code may be one subroutine or many, possibly constituting whole science modules. The code may originate from “scratch,” or be extracted from the repository and modified.
    2.  After testing or reaching a point of being satisfied with his/her results, he/she decides to save it in the repository so that others can have access to it.
    3.  Some archived codes may still be in an experimental, or development, state, while others may be reasonably stable and more completely tested. The latter may be designated as “released.” There is no enforceable means to control access based on an experimental or released state. The community will have, and should have, access indiscriminately, well aware that using development-state code is risky.
    4.  As the user continues to work with the codes, he/she may make enhancements or discover and fix errors. The upgrades are then installed in the repository, which automatically assigns unique revision identifiers.
    5.  The repository is located where it is conveniently accessible to all users, and is maintained by an administrator who sets and enforces general access rules.

### Choice of a configuration-management tool

Prior to CMAQ version=5.0.2, CMAQ developers used [CVS](https://en.wikipedia.org/wiki/Concurrent_Versions_System) for versioning, and distributed tarballs included CVS artifacts (e.g., files with names ending with ',v'). Starting with version=5.0.2, CMAQ developers switched to [git](https://en.wikipedia.org/wiki/Git_%28software%29).

#### CVS explained

There are many configuration management tools, both free and commercially available. We chose The Concurrent Versions System (CVS) mainly because of its versatility. CVS controls the concurrent editing of sources by several users working on releases built from a hierarchical set of directories. CVS uses the Revision Control System (RCS) as the base system. Other reasons that CVS was an attractive choice include the following:

-   It works on virtually all UNIX and Linux platforms and on many PCs.
-   It is publicly available and free.

The CVS wiki states that “CVS uses a client-server architecture: a server stores the current version(s) of a project and its history, and clients connect to the server in order to ‘check out’ a complete copy of the project, work on this copy and then later ‘check in’ their changes. Typically, the client and server connect over a LAN or over the Internet, but client and server may both run on the same machine if CVS has the task of keeping track of the version history of a project with only local developers. The server software normally runs on UNIX and Linux.

“Several developers may work on the same project concurrently, each one editing files within their own ‘working copy’ of the project, and sending (or checking in) their modifications to the server. To avoid the possibility of people stepping on each other's toes, the server will only accept changes made to the most recent version of a file. Developers are therefore expected to keep their working copy up-to-date by incorporating other people’s changes on a regular basis. This task is mostly handled automatically by the CVS client, requiring manual intervention only when a conflict arises between a checked-in modification and the yet-unchecked local version of a file.” Thus, CVS adds power and features that are attractive for the CMAQ system.

#### The CVS repository

The CVS repository structure, i.e., the UNIX directory hierarchy, follows the class/module organ­ization discussed in Young (1999). The repository is actually divided into many reposi­tories, one for each generic model. This division makes it easier to maintain the class/module organization that is important for the model-building operation described in Chapter 8. CVS allows for the use of a “modules” file,[2] which enables a user to easily check out or extract a complete CMAQ module. For example, a user might check out a module to make code modifications. Complete modules are checked out during the CMAQ model building operation. The following shows a small portion of a symbolic CVS UNIX directory tree that represents the current structure for CCTM:

+-\> CCTM

+-\> CVSROOT *(CVS administrative files)*

+-\> src

+-\> adjcon

| +-\> adjcon\_noop --\> RCS files

| +-\> denrate --\> RCS files

+-\> aero

| +-\> aero\_noop --\> RCS files

| +-\> aero4 --\> RCS files

| +-\> aero5 --\> RCS files

| +-\> aero5\_txhg --\> RCS files

+-\> aero\_depv

| +-\> aero\_depv\_noop --\> RCS files

| +-\> aero\_depv2 --\> RCS files

+-\> chem

| +-\> chem\_noop --\> RCS files

| +-\> smvgear --\> RCS files

| +-\> ros3 --\> RCS files

| +-\> ebi\_cb05cltx\_ae5 --\> RCS files

| +-\> ebi\_cb05cltxhg\_ae5 --\> RCS files

| +-\> ebi\_saprc99 --\> RCS files

| +-\> ebi\_saprc99 --\> RCS files

The symbolic tree is shown relative to the subdirectory in the repository named for the CCTM model. Similar trees exist for each of the generic models. The RCS files are the revision control history files that contain the change tables to reconstruct the actual source code according to a specific revision identifier. The tree closely follows the organization of classes and modules for CCTM and contains alternate modules within the classes. In particular, most classes contain a “no-operation” (noop) module that allows a user to essentially turn off that particular science process modeling. This is useful, for example, in debugging, where rapid turnaround is important, and a computationally demanding module that is not needed can be bypassed.

Guidelines for Developing New CMAQ Source Code
----------------------------------------------

### Object-oriented concepts

To make the CMAQ system robust and flexible, object-oriented concepts were incorporated into the design of the system. The incorporation of these ideas helps developers avoid introducing errors when code modifications are needed. Additionally, the system can easily and efficiently be modified, allowing the user to quickly create models for different applications. The implemen­tation language for CMAQ is Fortran 90, which imposes limits on how far one can go in terms of object-oriented design. In particular, because Fortran is a static language, objects cannot be instantiated dynamically; they must be declared explicitly in the source code to be created at compile time. However, to encourage a user community that will be contributing code for future enhancements, every attempt has been made to adhere to the Fortran 90 standard.

### Global name data table

To implement modularity and data independence, we have employed design ideas that draw heavily from the object-oriented concept of ''inheritance ''and code re-use. The data structures in the codes that deal with the chemical mechanism, I/O API, logical file names, general constants, and pointers are determined by Fortran declarations in data and parameter statements in the CMAQ system. These data structures pertain to a particular application and are meant to apply globally—not just to one particular CCTM through all its subroutines, but also to all the models that supply data to CCTM for that application. These data structures are contained in Fortran INCLUDE files, which are essentially header files, included in the declaration sections near the top of the Fortran code source files. The inclusion of these source files is made automatic by using a generic string that represents the INCLUDE file and that is parsed and expanded to the actual INCLUDE file during a preprocessing stage in the compilation. The Fortran global INCLUDE files contain name tables that define:

1.  The chemical mechanism;
2.  The I/O API interface, including logical file names;
3.  The global modeling constants; and
4.  Other constants or parameters that apply across the model.

To effect the implementation of the INCLUDE files into the code, a special compiling system, Bldmake, was developed (Fine et al., 1998), which reads a configuration file that, based on the application, completely determines the model executable to be built. The ASCII configuration file can be generated either by the CMAQ system or by the users following a few, simple syntactical rules. In addition to the global INCLUDE files, the configuration file contains module commands that tell Bldmake to extract the codes for that module from the model code repository for compilation.

### Thin Interface

As mentioned in Section 9.2.2, CMAQ is designed to be robust and flexible with respect to the interchange of modules and the elimination of cross-module data dependencies. Consequently, the concept of a “thin interface” has been employed in the design, which applies principally to the class-drivers (i.e. the top level call to a science module). At a minimum, the thin interface implementation implies the following requirements:

-   Eliminate global memory references (across modules). This implies no common blocks across modules, no hidden data paths, and no “back doors.”
-   Each module reads and interpolates its required data independently. The I/O API helps to ensure this kind of data independence.
-   Standardized argument list (CGRID, Date, Time, TimeStep) for calling the class-driver. See the example in Section 9.2.6. These requirements attempt to incorporate the object-oriented idea of encapsulation in the CMAQ design. Rumbaugh et al. (1991) suggest that “Encapsulation (also information hiding) consists of separating the external aspects of an object, which are accessible to other objects, from the internal implementation details of the object, which are hidden from other objects. Encapsulation prevents a program from becoming so interdependent that a small change has massive ripple effects. The implementation'' ''of an object can be changed without affecting the applications that use it.”

The encapsulation design makes the CMAQ system safer and enables the transaction processing, plug-and-play capability. This design also makes it easier for a user to trace data and usage within a module, particularly at the class-driver level.

### Coding guidelines

To maintain the object-oriented concepts implemented in the CMAQ system design, we have established a small set of coding guidelines that apply to those who develop CMAQ science modules and affect the low-level design of the models. We have developed standards to control data depen­dencies at the class-driver level, but we have not propagated these coding standards to the submodule level.

1.  The models are generally coded in Fortran (both Fortran 90 and Fortran 77 conventions are used by various developers). It is possible to link in subroutines written in the C language, although this has not been done within the current CMAQ implementation. While the Fortran 90 compiler will compile Fortran 77 code, the reverse is not true. Thus the Makefiles are set up to invoke the Fortran 90 compiler.
2.  To enable code compatibility between the Fortran 77 compiler and Fortran 90 code, the following guidance is provided: Line length beyond 72 characters is permissible in Fortran 90 (with line continuation indicated by an ending ‘&’), but not in Fortran 77; therefore, insertion of the ‘&’ in column 73 of the first line and in column 6 of the next line of the Fortran 90 code will ensure compatibility with both compilers (the ‘&’ at the beginning of a line is “in principle” ignored by the Fortran 90 compiler, but interpreted as a continuation character by the Fortran 77 compiler if it appears in column 6).
3.  The modules must be controlled by a top-level class-driver routine, whose calling arguments must be the computational concentration grid array (CGRID), the current scenario date (Date), scenario time (Time), and the controlling time step vector (TimeStep). (See Section 9.2.3 above.)
4.  The class-driver is also responsible for any temporal integration required within the module. (The time steps for process integration at the module level are usually shorter than those of the CCTM synchronization time step.)
5.  Any reads and writes for the module should be done at the level of the class-driver routine. Although not absolutely necessary, this is strongly suggested because it is usually much easier to control the timing of the data accesses at the highest level of the module where the current scenario date and time are known.
6.  Use the Fortran declaration IMPLICIT NONE to maintain some control on typographic errors and undefined variables. The use of IMPLICIT NONE forces the developer to declare all internal variables. This is standard in Fortran 90.
7.  Use the global INCLUDE files for chemical mechanism data, and other data where available.
8.  Use the I/O API for external data references where appropriate. For an illustration of these rules, see the code template provided in Section 9.2.6.

At the submodule level, there are no strict I/O or coding standards. Here it is envisioned that individual researchers/programmers use their own coding styles for their algorithms. However, the following suggestions are offered to facilitate the potential incorporation of a module into the CMAQ system:

-   In general, it is expected that MKS units are used for input and output variables, as these units have been standardized throughout the CMAQ system. Within a submodule subroutine, whatever units are most convenient can be used. However, the developer must be responsible for any unit conversions to MKS for input and output, and thus avoid potential errors.
-   For efficiency and performance considerations, operations may need to be done on groups of grid cells (a block of cells) at a time. If there are ''N ''cells in the block and the entire domain contains ''M ''cells, then the entire domain can be decomposed into ''M/N *blocks. The default value of N is set to 500. For operations in the horizontal (*x,y''), the cell constraint becomes X×Y≤N, where ''X *= number of cells in the*x''-direction, and *Y*= number of cells in the y-direction. For operations in both the horizontal and vertical, the constraint becomes X×Y×Z≤N, where ''Z *= number of cells in the*z''-direction. There may be some operations, such as for some horizontal advection schemes, where this decomposition into blocks becomes more difficult or impossible.

### Documentation guidelines

Appropriate documentation is critical to the ease of use and maintainability of code developed for CMAQ. The official released version of CMAQ contains extensive in-line documentation and references to pertinent technical information whenever possible. Given the increasing number of new developers and code modules, the following guidelines are provided for new code developed for CMAQ:

-   The code revision history should be initiated or updated as appropriate for new and modified code, indicating the author, date, and nature of the revision. The revision history appears at the top of the subroutine.
-   Complete references to the pertinent technical documents should be provided whenever possible, and listed in comment lines immediately following the revision history notes. They should be cited in comments preceding, or embedded in-line with, the relevant code segments.
-   In-line documentation of the variable definitions indicating units is highly recommended in both subroutines and INCLUDE files, to facilitate the correct implementation of any code modifications in the future. This information is generally included in comments embedded in-line with the declaration of each variable.

### Science process code template

The following example from CMAQ v4.7 illustrates a science process class-driver Fortran 90 subroutine. Code developers should follow this template, where appropriate, to maximize the benefit from the design concepts implemented in CMAQ. This template is generic and demonstrates many of the available features. Some class drivers and most other subprograms within a module may not have, nor require, most or any of these features. (The numbers at the left-hand margin refer to footnotes and are not part of the code, and the text within “\< \>” indicates code removed from the example for brevity in this section)

<center>
**Example of Science Process Class-Driver**

</center>
SUBROUTINE VDIFF ( CGRID, JDATE, JTIME, TSTEP )

( 1)C-----------------------------------------------------------------------

( 1)C Function:

( 1)C Preconditions:

( 1)C Subroutines and Functions Called:

( 1)C Revision History:

( 1)C References:

C-----------------------------------------------------------------------

( 2) USE AERO\_EMIS ! inherits GRID\_CONF

( 2) USE SUBST\_MODULES ! stenex

! USE SUBST\_GLOBAL\_SUM\_MODULE ! stenex

( 3) IMPLICIT NONE

! INCLUDE SUBST\_HGRD\_ID ! horizontal dimensioning parameters

! INCLUDE SUBST\_VGRD\_ID ! vertical dimensioning parameters

( 4) INCLUDE SUBST\_RXCMMN ! model mechanism name

( 4) INCLUDE SUBST\_GC\_SPC ! gas chemistry species table

( 4) INCLUDE SUBST\_GC\_EMIS ! gas chem emis surrogate names and map table

( 4) INCLUDE SUBST\_GC\_DEPV ! gas chem dep vel surrogate names and map table

( 4) INCLUDE SUBST\_GC\_DDEP ! gas chem dry dep species and map table

INCLUDE SUBST\_GC\_DIFF ! gas chem diffusion species and map table

( 4) INCLUDE SUBST\_AE\_SPC ! aerosol species table''' '''

! INCLUDE SUBST\_AE\_EMIS ! aerosol emis surrogate names and map table

( 4) INCLUDE SUBST\_AE\_DEPV ! aerosol dep vel surrogate names and map table

( 4) INCLUDE SUBST\_AE\_DDEP ! aerosol dry dep species and map table

( 4) INCLUDE SUBST\_AE\_DIFF ! aerosol diffusion species and map table

( 4) INCLUDE SUBST\_NR\_SPC ! non-reactive species table

( 4) INCLUDE SUBST\_NR\_EMIS ! non-react emis surrogate names and map table

( 4) INCLUDE SUBST\_NR\_DEPV ! non-react dep vel surrogate names and map table

( 4) INCLUDE SUBST\_NR\_DDEP ! non-react dry dep species and map table

( 4) INCLUDE SUBST\_NR\_DIFF ! non-react diffusion species and map table

( 4) INCLUDE SUBST\_TR\_SPC ! tracer species table

( 4) INCLUDE SUBST\_TR\_EMIS ! tracer emis surrogate names and map table

( 4) INCLUDE SUBST\_TR\_DEPV ! tracer dep vel surrogate names and map table

( 4) INCLUDE SUBST\_TR\_DDEP ! tracer dry dep species and map table

( 4) INCLUDE SUBST\_TR\_DIFF ! tracer diffusion species and map table

! INCLUDE SUBST\_EMLYRS\_ID ! emissions layers parameter

( 5)\#ifdef emis\_chem

( 5) INCLUDE SUBST\_EMPR\_CH ! emissions processing in chem

( 5)\#else

( 5) INCLUDE SUBST\_EMPR\_VD ! emissions processing in vdif

( 5)\#endif

( 6) INCLUDE SUBST\_PACTL\_ID ! PA control parameters

( 6) INCLUDE SUBST\_CONST ! constants

( 6) INCLUDE SUBST\_FILES\_ID ! file name parameters

( 6) INCLUDE SUBST\_IOPARMS ! I/O parameters definitions

\#include SUBST\_IODECL \# I/O definitions and declarations

! INCLUDE SUBST\_COORD\_ID ! coordinate and domain definitions (req IOPARMS)

( 7) CHARACTER( 120 ) :: XMSG = ' '

( 8)C Arguments:

! REAL CGRID( NCOLS,NROWS,NLAYS,\* ) ! concentrations

! REAL :: CGRID( :,:,:,: ) ! concentrations

( 8) REAL, POINTER :: CGRID( :,:,:,: ) ! concentrations

( 8) INTEGER JDATE ! current model date, coded YYYYDDD

( 8) INTEGER JTIME ! current model time, coded HHMMSS

( 8) INTEGER TSTEP( 2 ) ! time step vector (HHMMSS)

`! TSTEP(1) = local output step`

`! TSTEP(2) = sciproc sync. step (chem)`

( 9)C Parameters:

( 9)C explicit, THETA = 0, implicit, THETA = 1

( 9) REAL, PARAMETER :: THETA = 0.5, ! Semi-implicit (Crank-Nicolson)

( 9) & THBAR = 1.0 - THETA

( 9) REAL THRAT ! THBAR/THETA

( 9) INTEGER, PARAMETER :: N\_SPC\_DDEP = N\_GC\_DDEP

( 9) & + N\_AE\_DDEP

( 9) & + N\_NR\_DDEP

( 9) & + N\_TR\_DDEP

( 9)\< \>

( 9)C number of species on the PM emissions input file. Set in OPEMIS

( 9)C the value changes with the type of emissions file.

( 9) INTEGER, SAVE :: NAESPCEMIS

( 9) REAL, PARAMETER :: M2PHA = 1.0E+04 ! 1 hectare = 1.0e4 m\*\*2

( 9) REAL, PARAMETER :: CMLMR = 1.0E+06 ! ppmV/Molar Mixing Ratio

( 9) REAL, PARAMETER :: CNVTD = M2PHA / CMLMR / MWAIR ! combined ddep

! conversion factor

( 9) REAL, PARAMETER :: GPKG = 1.0E+03 ! g/Kg

( 9) REAL, PARAMETER :: MGPG = 1.0E+06 ! micro-g/g

(10)C External Functions not previously declared in IODECL3.EXT:

(10) INTEGER, EXTERNAL :: SECSDIFF, SEC2TIME, TIME2SEC

(10) LOGICAL, EXTERNAL :: ENVYN

(11)C File variables:

(11)\< \>

(12)C Local Variables:

(12) CHARACTER( 16 ), SAVE :: PNAME = 'VDIFFIM'

(12)\< \>

(12) REAL, ALLOCATABLE, SAVE :: VDEMIS( :,:,:,: ) ! total emissions array

(12)\< \>

(13) INTERFACE

(13) SUBROUTINE RDMET( MDATE, MTIME, RDEPVHT, RJACM, RVJACMF, RRHOJ, DENS1 )

(13) IMPLICIT NONE

(13) INTEGER, INTENT( IN ) :: MDATE, MTIME

(13) REAL, INTENT( OUT ) :: RDEPVHT( :,: )

(13) REAL, INTENT( OUT ) :: RJACM ( :,:,: )

(13) REAL, INTENT( OUT ) :: RVJACMF( :,:,: )

(13) REAL, INTENT( OUT ) :: RRHOJ ( :,:,: )

(13) REAL, INTENT( OUT ) :: DENS1 ( :,: )

(13) END SUBROUTINE RDMET

(13) SUBROUTINE RDDEPV ( MDATE, MTIME, MSTEP, CGRID, DEPV )

(13) IMPLICIT NONE

(13) INTEGER, INTENT( IN ) :: MDATE, MTIME, MSTEP

(13) REAL, POINTER :: CGRID( :,:,:,: )

(13) REAL, INTENT( OUT ) :: DEPV( :,:,: )

(13) END SUBROUTINE RDDEPV

(13)\< \>

END INTERFACE

C-----------------------------------------------------------------------

(14) IF ( FIRSTIME ) THEN

(14) FIRSTIME = .FALSE.

(14) LOGDEV = INIT3()

(14) C for emissions (from COORD.EXT) .......................................

(14) IF ( GDTYP\_GD .EQ. LATGRD3 ) THEN

(14) DX1 = DG2M \* XCELL\_GD ! in m.

(14) DX2 = DG2M \* YCELL\_GD

(14) & \* COS( PI180\*( YORIG\_GD + YCELL\_GD \* FLOAT( GL\_NROWS/2 ))) ! in m.

(14) ELSE

(14) DX1 = XCELL\_GD ! in m.

(14) DX2 = YCELL\_GD ! in m.

(14) END IF

(14) C create global maps

(14) CALL VDIFF\_MAP ( DF2EM, DF2DV, DD2DV, DEPV\_MAP, DIFF\_MAP, DDEP\_SPC,

(14) & DV2DF )

(14) C set vertical layer definitions from COORD.EXT

(15) ALLOCATE ( RDX3F( NLAYS ), STAT = ALLOCSTAT )

(15) ALLOCATE ( RDX3M( NLAYS ), STAT = ALLOCSTAT )

(15) IF ( ALLOCSTAT .NE. 0 ) THEN

(15) XMSG = 'Failure allocating RDX3F or RDX3M'

(15) CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )

(15) END IF

(14) \< other calculations that need to be performed only the first time \>

`END IF ! if Firstime`

(16) MDATE = JDATE

(16) MTIME = JTIME

(16) MSTEP = TIME2SEC( TSTEP( 2 ) )

(16) DTSEC = FLOAT( MSTEP )

(16) CALL NEXTIME ( MDATE, MTIME, SEC2TIME( MSTEP / 2 ) )

C read & interpolate met data

(17) CALL RDMET ( MDATE, MTIME, RDEPVHT, RJACM, RVJACMF, RRHOJ, DENS1 )

C read & interpolate deposition velocities

`< perform other operations > `

(18) IF ( LIPR ) THEN

(18) DO S = 1, N\_SPC\_EMIS+1

(18) DO L = 1, ELAYS

(18) DO R = 1, MY\_NROWS

(18) DO C = 1, MY\_NCOLS

(18) EMIS\_PA( C,R,L,S ) = VDEMIS( S,L,C,R )

(18) END DO

(18) END DO

(18) END DO

(18) END DO

(18) CALL PA\_UPDATE\_EMIS ( 'VDIF', EMIS\_PA, JDATE, JTIME, TSTEP )

(18) END IF

(19) CALL EDYINTB ( EDDYV, DT, JDATE, JTIME, TSTEP( 2 ) )

\< Perform other operations to set up for tridiagonal solver \>

(20) DO 345 R = 1, MY\_NROWS

(20) DO 344 C = 1, MY\_NCOLS

\< Perform operations \>

(21) DO 301 N = 1, NSTEPS( C,R )

\< Perform operations \>

(21) 301 CONTINUE ! end time steps loop

\< Update concentration and deposition arrays \>

(20) 344 CONTINUE ! end loop on col C

(20) 345 CONTINUE ! end loop on row R

\< Perform other operations \>

C If last call this hour: write accumulated depositions:

(22) WSTEP = WSTEP + TIME2SEC( TSTEP( 2 ) )

(22) IF ( WSTEP .GE. TIME2SEC( TSTEP( 1 ) ) ) THEN

(22) MDATE = JDATE

(22) MTIME = JTIME

(22) CALL NEXTIME( MDATE, MTIME, TSTEP( 2 ) )

(22) WSTEP = 0

(22) DO V = 1, N\_SPC\_DDEP

(22) S = DD2DV( V )

(22) DO R = 1, MY\_NROWS

(22) DO C = 1, MY\_NCOLS

(22) WRDD( C,R ) = DDEP( S,C,R )

(22) END DO

(22) END DO

(22) IF ( .NOT. WRITE3( CTM\_DRY\_DEP\_1, DDEP\_SPC( V ),

(22) & MDATE, MTIME, WRDD ) ) THEN

(22) XMSG = 'Could not write ' // CTM\_DRY\_DEP\_1 // ' file'

(22) CALL M3EXIT( PNAME, MDATE, MTIME, XMSG, XSTAT1 )

(22) END IF

(22) END DO

(18) EMIS\_PA( C,R,L,S ) = VDEMIS( S,L,C,R )

(18) END DO

(18) END DO

(18) END DO

(18) END DO

(18) CALL PA\_UPDATE\_EMIS ( 'VDIF', EMIS\_PA, JDATE, JTIME, TSTEP )

(18) END IF

(19) CALL EDYINTB ( EDDYV, DT, JDATE, JTIME, TSTEP( 2 ) )

\< Perform other operations to set up for tridiagonal solver \>

(20) DO 345 R = 1, MY\_NROWS

(20) DO 344 C = 1, MY\_NCOLS

\< Perform operations \>

(21) DO 301 N = 1, NSTEPS( C,R )

\< Perform operations \>

(21) 301 CONTINUE ! end time steps loop

\< Update concentration and deposition arrays \>

(20) 344 CONTINUE ! end loop on col C

(20) 345 CONTINUE ! end loop on row R

\< Perform other operations \>

C If last call this hour: write accumulated depositions:

(22) WSTEP = WSTEP + TIME2SEC( TSTEP( 2 ) )

(22) IF ( WSTEP .GE. TIME2SEC( TSTEP( 1 ) ) ) THEN

(22) MDATE = JDATE

(22) MTIME = JTIME

(22) CALL NEXTIME( MDATE, MTIME, TSTEP( 2 ) )

(22) WSTEP = 0

(22) DO V = 1, N\_SPC\_DDEP

(22) S = DD2DV( V )

(22) DO R = 1, MY\_NROWS

(22) DO C = 1, MY\_NCOLS

(22) WRDD( C,R ) = DDEP( S,C,R )

(22) END DO

(22) END DO

(22) IF ( .NOT. WRITE3( CTM\_DRY\_DEP\_1, DDEP\_SPC( V ),

(22) & MDATE, MTIME, WRDD ) ) THEN

(22) XMSG = 'Could not write ' // CTM\_DRY\_DEP\_1 // ' file'

(22) CALL M3EXIT( PNAME, MDATE, MTIME, XMSG, XSTAT1 )

(22) END IF

(22) END DO

(22) WRITE( LOGDEV, '( /5X, 3( A, :, 1X ), I8, ":", I6.6 )' )

(22) & 'Timestep written to', CTM\_DRY\_DEP\_1,

(22) & 'for date and time', MDATE, MTIME

(18) IF ( LIPR ) THEN

! DO V = 1, N\_SPC\_DDEP

(18) DO V = 1, N\_SPC\_DEPV

(18) DO R = 1, MY\_NROWS

(18) DO C = 1, MY\_NCOLS

(18) DDEP\_PA( C,R,V ) = DDEP( V,C,R )

(18) END DO

(18) END DO

(18) END DO

(18) CALL PA\_UPDATE\_DDEP ( 'VDIF', DDEP\_PA, JDATE, JTIME, TSTEP )

(18) END IF

C re-set dry deposition array to zero

DDEP = 0.0

END IF

(23) RETURN

(23) END

**Footnotes:**

*( 1)Header comments - Highly recommended for internal documentation.*

*( 2)USE \<module name\> includes the Fortran source file specified.*

*( 3)IMPLICIT NONE must be used in Fortran 90, i.e., implicit declarations are not supported. This dramatically reduces errors due to typos and undefined variables.*

*( 4)Chemical mechanism array dimensioning and looping global variables.*

*( 5)C preprocessor flags that determine which emissions control dimensioning and looping variables are compiled.*

*( 6)Other global array dimensioning and looping global variables, including those for the I/O API. The logical variable LIPR is defined in the SUBST\_PACTL\_ID INCLUDE file for use at lines labeled (18).*

*( 7)Local variable declaration. Note syntax differences from Fortran-77.*

*( 8)Declarations for the argument list (standardized).*

*( 9)Declarations and PARAMETER statements for local Fortran parameters, illustrating in-line documentation of variables and units. Note syntax differences from Fortran-77.*

*(10)Declarations for external functions not previously declared.*

*(11)Declarations for arrays to hold external file data.*

*(12)Declarations and definitions for local and saved variables, and dynamic memory allocations.*

*(13)Interface is a convenient way to declare calling arguments to a subroutine as input, output, or both in the calling program through the INTENT variable specification (as IN, OUT, or IN OUT). No other declaration of the calling arguments is necessary in the calling program. If IN only, the values of arguments can be passed explicitly in the subroutine call. If OUT, the argument must be passed as a variable.*

*(14)Code section for subroutine initialization and for any local data that need not be set at every entry into the subroutine. Such data would require a SAVE statement in the declarations. For example, FIRSTIME is initialized to .TRUE. in the local variables section.*

*(15)Illustration of memory allocation for a variable declared as allocatable. In this example, NLAYS is accessed from the COORD.EXT file.*

*(16)Illustrates using an I/O API function to set file interpolation time.*

*(17)Meteorological and other data are read and interpolated through a series of subroutine calls. These subroutines in turn use I/O API utilities to perform the time interpolation of the desired met variables, deposited and emitted species.*

*(18)Call to process analysis routine to obtain data for the optional integrated process rates function.*

*(19)Illustrates call to another science process within the module.*

*(20)Main computational loop over the horizontal grid.*

*(21)Time-step loop over subsynchronization time step intervals.*

*(22)Illustrates writing to an I/O API file within a module.*

*(23)Subroutine end*

Compiling CMAQ with New Source Code
-----------------------------------

The following steps are recommended for compiling CMAQ when a new module has been developed. The procedure creates a Makefile, which can then be modified to add the new module in the appropriate class, but the same steps can be used to obtain a configuration file that can be similarly modified to add the new module.

-   On the computational platform of choice, create a working directory for the model download.
-   Download the appropriate tar file CMAQv5.tar.gz from the CMAS web site ([www.cmascenter.org](http://www.cmascenter.org/)) for the chosen platform. Users must register before proceeding with the download steps.
-   Untar the file using the command:

\> tar xvfz CMAQv5.tar.gz

This will expand a directory labeled *scripts* that contains all the scripts necessary to compile and run CMAQ.

-   Either install the CMAQ source code and libraries (Chapter 3) or create links to the CMAQ models and libraries as follows:

\> ln –s \<models directory\> models

\> ln –s \<lib directory\> lib

-   In the scripts/cctm subdirectory, modify a file called bldit.cctm as follows:

uncomment the line “set MakeOpt” by removing the leading ‘\#’ character.

-   Execute the bldit.cctm script. This creates a Makefile as well as a configuration file in the subdirectory scripts/cctm/BLD\_V5f, where the model code has been copied.
-   The Makefile can be modified to compile and link the new module by specifying \<full path name\>.o for the object file that needs to be linked in. It is essential that a source file with the corresponding name (with extension “.F”) reside in the same directory as the specified path name for the object file.
-   Issue the “make” command to compile the source code into an executable.

\> make –f Makefile

Guidelines to Writing Shell Scripts for CMAQ
--------------------------------------------

To run a model executable, various UNIX environment variables must be set in the shell that invokes the execute command. Generally, these variables involve the modeling scenario start date and time, the run duration, the output time step interval, various internal code flags that differ among the models, and all the input and output logical (symbolic) file names. There are various ways that external file names can be referenced in the source code, and UNIX platforms can link them by using environment variables. There are I/O API utility functions that allow users to easily access these variables in the code in a generic and portable manner. An additional feature that is provided through the I/O API is the ability to declare a file “volatile” by appending a -v flag in the shell’s declaration for the environment variable. By doing this, the I/O API will cause the netCDF file to update (sync) its disk copy after every write and thereby update the netCDF header. Otherwise, netCDF (I/O API) file headers are not updated until the files are closed. This feature is useful, for example, for allowing a user to analyze an open netCDF file using visualization tools while the model is executing. It is also useful in case of a system crash. A CCTM model can be restarted at the scenario time step after the last successful write using the aborted output file as the input initial data.

The following is a sample run script that can be downloaded from the CMAS web site. The build and run scripts are part of the downloaded tar file from this site.

\#! /bin/csh –f

\# ======================== CCTMv4.7 Run Script ====================== \#

\# Usage: run.cctm \>&! cctm\_e3a.log & \#

\# The following environment variables must be set for this script to \#

\# execute properly: \#

\# setenv M3DATA = input/output data directory \#

\# To report problems or request help with this script/program: \#

\# <http://www.cmascenter.org/html/help.html> \#

\# =================================================================== \#

\#\> Check that M3DATA is set:

if ( ! -e \$M3DATA ) then

echo " \$M3DATA path does not exist"

exit 1

endif

echo " "; echo " Input data path, M3DATA set to \$M3DATA"; echo " "

set APPL = e3a

set CFG = e3a

\#set CFG = \$APPL

set EXEC = CCTM\_\$CFG \# ctm version

\#\> horizontal domain decomposition

\#setenv NPCOL\_NPROW "1 1"; set NPROCS = 1 \# single processor setting

setenv NPCOL\_NPROW "4 2"; set NPROCS = 8

\#\> for Scyld Beowulf ...

\#setenv NP \$NPROCS

\#setenv BEOWULF\_JOB\_MAP -1:-1:0:0:1:1:2:2:3:3:4:4

\#echo " task-processor map \`beomap\`"

\#\> Set the working directory:

set BASE = \$cwd

cd \$BASE; date; cat \$BASE/cfg.\$CFG; echo " "; set echo

\#\> timestep run parameters

set STDATE = 2001203 \# beginning date

set STTIME = 000000 \# beginning GMT time (HHMMSS)

set NSTEPS = 240000 \# time duration (HHMMSS) for this run

set TSTEP = 010000 \# output time step interval (HHMMSS)

\#\> set log file [ default = unit 6 ]; uncomment to write standard output to a log

\#setenv LOGFILE \$BASE/\$APPL.log

\#\> turn off excess WRITE3 logging

setenv IOAPI\_LOG\_WRITE F

\#\> max sync time step (sec) (default is 720)

\#setenv CTM\_MAXSYNC 300

\#\> aerosol diagnostic file [ T | Y | F | N ] (default is F|N)

\#setenv CTM\_AERDIAG Y

\#\> sea-salt emissions diagnostic file [ T | Y | F | N ] (default is F|N)

\#setenv CTM\_SSEMDIAG Y

\#\> stop on inconsistent input file [ T | Y | F | N ]

setenv FL\_ERR\_STOP F

\#\> remove existing output files?

\#set DISP = delete

\#set DISP = update

set DISP = keep

\#\> output files and directories

set OUTDIR = \$M3DATA/cctm

if ( ! -d "\$OUTDIR" ) mkdir -p \$OUTDIR

set CONCfile = \$EXEC"CONC".\$APPL \# CTM\_CONC\_1

set ACONCfile = \$EXEC"ACONC".\${APPL} \# CTM\_ACONC\_1

set CGRIDfile = \$EXEC"CGRID".\${APPL} \# CTM\_CGRID\_1

set DD1file = \$EXEC"DRYDEP".\$APPL \# CTM\_DRY\_DEP\_1

set WD1file = \$EXEC"WETDEP1".\$APPL \# CTM\_WET\_DEP\_1

set WD2file = \$EXEC"WETDEP2".\$APPL \# CTM\_WET\_DEP\_2

set SS1file = \$EXEC"SSEMIS1".\$APPL \# CTM\_SSEMIS\_1

set AV1file = \$EXEC"AEROVIS".\$APPL \# CTM\_VIS\_1

set AD1file = \$EXEC"AERODIAM".\$APPL \# CTM\_DIAM\_1

set PA1file = \$EXEC"PA\_1".\$APPL \# CTM\_IPR\_1

set PA2file = \$EXEC"PA\_2".\$APPL \# CTM\_IPR\_2

set PA3file = \$EXEC"PA\_3".\$APPL \# CTM\_IPR\_3

set IRR1file = \$EXEC"IRR\_1".\$APPL \# CTM\_IRR\_1

set IRR2file = \$EXEC"IRR\_2".\$APPL \# CTM\_IRR\_2

set IRR3file = \$EXEC"IRR\_3".\$APPL \# CTM\_IRR\_3

set RJ1file = \$EXEC"RJ\_1".\$APPL \# CTM\_RJ\_1

set RJ2file = \$EXEC"RJ\_2".\$APPL \# CTM\_RJ\_2

\#\> set ancillary log file name extensions

setenv CTM\_APPL \$APPL

\#\> set floor file (neg concs)

setenv FLOOR\_FILE \$BASE/FLOOR\_\${APPL}

\#\> horizontal grid defn; check GRIDDESC file for GRID\_NAME options

setenv GRIDDESC ../GRIDDESC1

setenv GRID\_NAME M\_36\_2001

\#\> species for standard conc

\#setenv CONC\_SPCS "O3 NO ANO3I ANO3J NO2 FORM ISOP ANH4J ASO4I ASO4J"

\#\> layer range for standard conc

\#setenv CONC\_BLEV\_ELEV " 1 4"

\#\> species for integral average conc

setenv AVG\_CONC\_SPCS "O3 NO CO NO2 ASO4I ASO4J NH3"

\#setenv AVG\_CONC\_SPCS "ALL"

\#\> layer range for integral average conc

setenv ACONC\_BLEV\_ELEV " 1 1"

\#\> input files and directories

set OCEANpath = \$M3DATA/emis/2001

set OCEANfile = us36\_surf.40x44.ncf

set EMISpath = \$M3DATA/emis/2001

set EMISfile = emis3d.20010722.US36\_40X44.ncf

\#set TR\_EMpath =

\#set TR\_EMfile =

\#set GC\_ICpath = \$OUTDIR

\#set GC\_ICfile = CCTM\_e3aCGRID.d1b

set GC\_ICpath = \$M3DATA/icon

set GC\_ICfile = ICON\_cb05cl\_M\_36\_2001\_profile

set GC\_BCpath = \$M3DATA/bcon

set GC\_BCfile = BCON\_cb05cl\_M\_36\_2001\_profile

set METpath = \$M3DATA/mcip3/M\_36\_2001

set extn = 010722

set GC2file = GRIDCRO2D\_\${extn}

set GD2file = GRIDDOT2D\_\${extn}

set MC2file = METCRO2D\_\${extn}

set MD3file = METDOT3D\_\${extn}

set MC3file = METCRO3D\_\${extn}

set MB3file = METBDY3D\_\${extn}

set TR\_DVpath = \$METpath

set TR\_DVfile = \$MC2file

\#\> 7-level photolysis data w/ file header

set JVALpath = \$M3DATA/jproc

set JVALfile = JTABLE\_\${STDATE}

set AE\_ICpath = \$GC\_ICpath

set NR\_ICpath = \$GC\_ICpath

set TR\_ICpath = \$GC\_ICpath

set AE\_ICfile = \$GC\_ICfile

set NR\_ICfile = \$GC\_ICfile

set TR\_ICfile = \$GC\_ICfile

set AE\_BCpath = \$GC\_BCpath

set NR\_BCpath = \$GC\_BCpath

set TR\_BCpath = \$GC\_BCpath

set AE\_BCfile = \$GC\_BCfile

set NR\_BCfile = \$GC\_BCfile

set TR\_BCfile = \$GC\_BCfile

\#\> input and output files and directories (boilerplate)

source in\_out.q

if ( \$status ) exit 1

\#\> for the run control ...

setenv CTM\_STDATE \$STDATE

setenv CTM\_STTIME \$STTIME

setenv CTM\_RUNLEN \$NSTEPS

setenv CTM\_TSTEP \$TSTEP

setenv CTM\_PROGNAME \$EXEC

\#\> look for existing log files

set test = \`ls CTM\_LOG\_???.\${APPL}\`

if ( "\$test" != "" ) then

if ( \$DISP == 'delete' ) then

echo " ancillary log files being deleted"

foreach file ( \$test )

echo " deleting \$file"

rm \$file

end

else

echo "\*\*\* Logs exist - run ABORTED \*\*\*"

exit 1

endif

endif

\#\> - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

env

ls -l \$BASE/\$EXEC; size \$BASE/\$EXEC

\#\> Executable call for single PE, uncomment to invoke

\# time \$BASE/\$EXEC

\#\> Executable call for multiple PE, set location of MPIRUN script

set MPIRUN = /share/linux/bin/mpich-ch\_p4/bin/mpirun

set TASKMAP = \$BASE/machines8

cat \$TASKMAP

time \$MPIRUN -v -machinefile \$TASKMAP -np \$NPROCS \$BASE/\$EXEC

date

exit

Testing and Distribution of Development Source Code
---------------------------------------------------

The CMAS Center collects, tests, and distributes various operational and development versions of CMAQ through the web site [<http://www.cmaq-model.org>](http://www.cmaq-model.org/). An archive of official releases (both current and past) and development versions of CMAQ is available to the user community. The CMAQ-MADRID and CMAQ-AMSTERDAM developed by AER, Inc. under funding from the Electric Power Research Institute can be downloaded from this archive. As a benefit to the CMAQ community, CMAS periodically updates its documentation on testing such development code versions to include additional feedback as it becomes available, based on users’ experiences with these versions. Questions or comments about development versions of CMAQ such as CMAQ-MADRID should be directed to the developers at AER. Questions or comments about downloading the source code and associated documentation, and on the software development guidelines, may be directed to [<http://www.cmascenter.org>](http://www.cmascenter.org/).

Based on the insights gained from the testing and archiving of a development version of the model such as CMAQ-MADRID, CMAS recom­mends the following steps as the minimum level of coding and testing practices to be adopted by developers wishing to contribute code to the public CMAQ archive:

1.  To make the best use of the CMAQ features in developing new code, the developer should review the coding conventions that are provided in the previous sections of this chapter. [Also see [<http://www.epa.gov/asmdnerl/CMAQ/CMAQscienceDoc.html>](http://www.epa.gov/asmdnerl/CMAQ/CMAQscienceDoc.html)].
2.  New code should be built using the current operational CMAQ version as a template whenever possible. This will facilitate consistency in coding practices, including naming conventions, in-line documentation, and the specification of compile time versus run-time parameters.
3.  Before submitting source code to the CMAS Center, the developer should verify that the code is consistent with the operational CMAQ version from which it was built, especially in the use of common INCLUDE files (such as horizontal and vertical grid definition files) and run-time parameter settings. Mixing code from different operational versions of the CMAQ model within the same development code version can lead to problems in using the generalized CMAQ scripts.
4.  Comprehensive documentation or other references to peer-reviewed literature should be provided for any new science algorithms include in the source code (see Section 9.2.5).
5.  The developer must document the computational platform used for the testing, including type and speed of the processor(s), the compiler version used, and CPU usage. It is recommended that developers use any combination of the above for testing code intended for release through the CMAS Center, to facilitate benchmarking and portability testing by CMAS staff. Any documentation on potential differences in model outputs between different computing platforms would be useful for end-users who may not be able to duplicate the platform on which the model was initially developed and tested. To this end, code testing and documentation of test results by developers, using more than one platform if available, are highly desirable.
6.  The developer should provide all input data for the test case so that interested users may attempt to run the code and reproduce the results on their own platforms.
7.  It is recommended that benchmark results from the testing be provided for at least one 5‑day simulation. Shorter simulations do not provide adequate results from which to discern model trends beyond the spin-up period.
8.  When making incremental changes to model science, the developer should provide documentation of the results, including (a) the results for all variables that show a deviation of greater than 1.0e10<sup>‑6</sup> ppm for the gas-phase species or 1.0e10<sup>‑4</sup> µg m<sup>‑3</sup> for the particulate species from the base model results for the same case, (b) an analysis of what was done to understand these differences, and (c) conclusions of the analysis.
9.  Note that more than one simulation may be necessary to adequately demonstrate seasonal or regional biases, if any, in the results. It is also understood that with models still under development, the analysis may not resolve all differences from the operational model results. It is recommended that these unresolved issues also be documented.

Model developers are also recommended to check the CMAS website to see if there are any additional guidelines that have been recommended since the first set listed above.

References
----------

Fine, S. S., W. T. Smith, D. Hwang, T. L. Turner, 1998: Improving model development with configuration management, IEEE Computational Science and Engineering, 5(1, Ja-Mr), 56-65.

J. Rumbaugh, M. Blaha, W. Premerlani, F. Eddy, and W. Lorensen, 1991: Object-Oriented Modeling and Design, Prentice Hall

Young, J. O.,'' ''Integration of Science Code into Models-3, 1999. In *Science Algorithms of the EPA Models-3 Community Multiscale Air Quality (CMAQ) Modeling System*, D. W. Byun and J. K. S. Ching (ed.), EPA/600/R-99/030, U. S. EPA, Research Triangle Park, NC.


[<< Previous Chapter](CMAQ_OGD_ch10_new_simulation) - [Home](CMAQ_OGD_index) - [Next Chapter >>](CMAQ_OGD_ch12_analysis_tools)
