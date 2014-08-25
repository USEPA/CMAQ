
C RCS file, release, date & time of last delta, author, state, [and locker]
C $Header: /project/work/rep/STENEX/src/se_snl/se_comm_info_ext.f,v 1.1 2004/03/26 16:16:47 yoj Exp $ 

C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%

C --------------------------------------------------------------------------
C Note: all these variables with prefix se_ are for stencil exchange library
C       only
C
C to define communication info variables:
C   
C   se_ngb_pe    -- an array to indicate a communication with a certain 
C                   processor is required base upon near-neighbour 
C                   communication pattern: -1 denotes no communication is 
C                   needed, and a non -1 number denotes processor number with 
C                   which communication is formed
C   se_numdim    -- dimensionality of a data structure which requires 
C                   communication
C   se_decompstr -- indicator of which dimenion(s) of data is/are decomposed, 
C                   0 (not decomposed), 1 (decomposed)
C --------------------------------------------------------------------------

        module se_comm_info_ext

          integer :: se_ngb_pe(8)
          integer :: se_numdim
          character (len=10) :: se_decompstr

          integer :: se_twoway_npcol, se_twoway_nprow

        end module se_comm_info_ext
