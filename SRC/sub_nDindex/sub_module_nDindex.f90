!===============================================================================
!===============================================================================
!This file is part of FOR_EVRT library.
!
!===============================================================================
! MIT License
!
! Copyright (c) 2022 David Lauvergnat
!
! Permission is hereby granted, free of charge, to any person obtaining a copy
! of this software and associated documentation files (the "Software"), to deal
! in the Software without restriction, including without limitation the rights
! to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
! copies of the Software, and to permit persons to whom the Software is
! furnished to do so, subject to the following conditions:
!
! The above copyright notice and this permission notice shall be included in all
! copies or substantial portions of the Software.
!
! THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
! IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
! FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
! AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
! LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
! OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
! SOFTWARE.
!===============================================================================
!===============================================================================
MODULE mod_nDindex
  USE QDUtil_m
  use mod_module_DInd, only: typedind, set_ndind_01order,             &
                             set_ndind_10order,                       &
                             set_ndind_01order_l, set_ndind_10order_l,&
                             dealloc_ndind, ndind2tondind1, write_tab_ndind
  IMPLICIT NONE

      PRIVATE

      integer, parameter :: err_Max_nDI = 1
      integer, parameter :: err_nDI     = 2
      integer, parameter :: err_nDval   = 3

      !!@description: TODO
      !!@param: TODO
      TYPE Type_nDindex

        logical                       :: alloc           = .FALSE. ! IF F, tables haven't been allocated.
        logical                       :: init            = .FALSE. ! IF F, tables haven't been initialized
        logical                       :: Write_Tab       = .FALSE. ! IF T write Tab_nDval and Tab_Norm

        integer                       :: ndim            = 0       ! number of index: dimension of table nDind, nDdim
        integer, allocatable          :: nDinit(:)                 ! smallest value of the individual index (1 or 0)
        integer, allocatable          :: nDend(:)                  ! largest value of the individual index
        integer, allocatable          :: nDsize(:)                 ! size of the individual index
        real(kind=Rkind), allocatable :: nDweight(:)               ! weight of the individual index
        logical, allocatable          :: skip_li(:,:)              ! skip_li(0:Lmax,ndim). Default F (keep all l(:) terms)

        logical                       :: packed          = .FALSE. ! IF T allocate and set the Tab_nDval and Tab_Norm
        logical                       :: packed_done     = .FALSE. ! IF T The pack feature is done

        integer, allocatable          :: Tab_nDval(:,:)            ! Tab_nDval(ndim,Max_nDI) table the individual indexes
        real(kind=Rkind), allocatable :: Tab_Norm(:)               ! TabNorm for the whole basis
        integer,  allocatable         :: Tab_L(:)                  ! Equivalent to Tab_Norm (but integer)

        integer                       :: Max_nDI         = 0       ! largest value of the multidimensional index

        integer                       :: type_OF_nDindex = -1
                                        ! 0: using a table such Sum(nDind(:)) < Norm
                                        ! 1: standard table, (init1....dim1)X(init2....dim2)....
                                        ! 2: such Sum(nDind(:)) < Norm
                                        ! 3: such Sum(nDind(:)) < Lmax (for new Smolyak grid)

        integer                       :: MaxCoupling     = -1
        integer                       :: MinCoupling     = 0
        real(kind=Rkind)              :: MaxNorm         = -ONE
        real(kind=Rkind)              :: MinNorm         = ZERO
        integer                       :: Lmax            = -1
        integer                       :: L1max           = huge(1)
        integer                       :: L2max           = huge(1)
        integer, allocatable          :: nDNum_OF_Lmax(:)            ! associated the index to Lmax (0), L1max (1), L2max(2)

        integer                       :: Lmin            = 0
        logical                       :: With_L          = .FALSE. ! IF T, it uses L instead of the Norm

        TYPE (RealVec_t), allocatable :: Tab_nDNorm(:)             ! Tab_nDindex(ndim) (for recursive used)
        TYPE (IntVec_t),  allocatable :: Tab_i_TO_l(:)             ! equivalent to Tab_nDNorm

        TYPE(TypeDInd),  allocatable  :: Tab_DInd(:) ! for SGtype2
      END TYPE Type_nDindex

        INTERFACE alloc_array
          MODULE PROCEDURE alloc_array_OF_nDindexdim0,alloc_array_OF_nDindexdim1
        END INTERFACE
        INTERFACE dealloc_array
          MODULE PROCEDURE dealloc_array_OF_nDindexdim0,dealloc_array_OF_nDindexdim1
        END INTERFACE

        INTERFACE alloc_NParray
          MODULE PROCEDURE alloc_NParray_OF_nDindexdim0,alloc_NParray_OF_nDindexdim1
        END INTERFACE
        INTERFACE dealloc_NParray
          MODULE PROCEDURE dealloc_NParray_OF_nDindexdim0,dealloc_NParray_OF_nDindexdim1
        END INTERFACE

        PUBLIC :: Type_nDindex,alloc_nDindex,dealloc_nDindex,Write_nDindex

        PUBLIC :: alloc_array,dealloc_array,alloc_NParray,dealloc_NParray
        PUBLIC :: init_nDindexPrim,init_nDindex_typeTAB
        PUBLIC :: pack_nDindex,sort_nDindex,unpack_nDindex
        PUBLIC :: calc_nDI,calc_Norm_OF_nDI,calc_Norm_OF_nDval,calc_L_OF_nDI,calc_L_OF_nDval
        PUBLIC :: calc_nDindex,calc_nDval_m1,calc_nDval_p1

        PUBLIC :: ADD_ONE_TO_nDindex,ADD_ONE_TO_nDval_m1,ADD_ONE_TO_nDval_p1
        PUBLIC :: init_nDval_OF_nDindex

      CONTAINS

      SUBROUTINE init_nDindexPrim(nDindex,ndim,nDsize,nDend,nDinit,             &
                                  nDweight,type_OF_nDindex,                     &
                                  MinNorm,MaxNorm,MaxCoupling,MinCoupling,      &
                                  Lmin,Lmax,L1max,L2max,nDNum_OF_Lmax,          &
                                  skip_li,                                      &
                                  tab_i_TO_l,With_init,With_nDindex,err_sub)

        TYPE (Type_nDindex), intent(inout)           :: nDindex
        integer,             intent(in)              :: ndim

        integer,             intent(in),    optional :: nDsize(:)
        integer,             intent(in),    optional :: nDend(:)

        integer,             intent(inout), optional :: err_sub
        integer,             intent(in),    optional :: type_OF_nDindex,MaxCoupling,MinCoupling
        integer,             intent(in),    optional :: nDinit(:),nDNum_OF_Lmax(:)
        real(kind=Rkind),    intent(in),    optional :: nDweight(:)
        real(kind=Rkind),    intent(in),    optional :: MaxNorm,MinNorm
        TYPE (IntVec_t),  intent(in),    optional, allocatable :: tab_i_TO_l(:)
        integer,             intent(in),    optional :: Lmin,Lmax,L1max,L2max
        logical,             intent(in),    optional :: With_init,With_nDindex

        logical,             intent(in),    optional :: skip_li(:,:)


        integer :: i,err_sub_loc
        logical :: With_init_loc,With_nDindex_loc

!-----------------------------------------------------------
      integer :: err_mem,memory
      character (len=*), parameter :: name_sub='init_nDindexPrim'
      logical,parameter :: debug=.FALSE.
      !logical,parameter :: debug=.TRUE.
!-----------------------------------------------------------
      IF (debug) THEN
        write(out_unit,*) 'BEGINNING ',name_sub
      END IF
!-----------------------------------------------------------
      err_sub_loc = 0

      IF (present(With_init)) THEN
        With_init_loc = With_init
      ELSE
        With_init_loc = .TRUE.
      END IF
      IF (present(With_nDindex)) THEN
        With_nDindex_loc = With_nDindex
      ELSE
        With_nDindex_loc = .TRUE.
      END IF

      ! with the initialization of the ndim size tables.
      IF (With_init_loc) THEN
        IF (nDindex%init) THEN
          write(out_unit,*) ' ERROR in ',name_sub
          write(out_unit,*) ' nDindex is already initialized!!'
          write(out_unit,*) ' Check the fortran source!!'
          STOP
        END IF

        CALL alloc_nDindex(nDindex,ndim)

        CALL alloc_NParray(nDindex%nDNum_OF_Lmax,[ndim],'nDindex%nDNum_OF_Lmax',name_sub)
        IF (present(nDNum_OF_Lmax)) THEN
          nDindex%nDNum_OF_Lmax(:) = nDNum_OF_Lmax
        ELSE
          nDindex%nDNum_OF_Lmax(:) = 0
          nDindex%L1max            = huge(1)
          nDindex%L2max            = huge(1)
        END IF

        nDindex%init = .TRUE.

        IF (present(Lmax) .AND. present(MaxNorm)) THEN
          write(out_unit,*) ' ERROR in ',name_sub
          write(out_unit,*) ' Both Lmax and MaxNorm are present'
          write(out_unit,*) '  It is not possible'
          write(out_unit,*) ' Check the fortran source!!'
          STOP
        END IF

        nDindex%With_L = present(Lmax)

        IF (nDindex%With_L) THEN
          CALL alloc_NParray(nDindex%skip_li,[Lmax,ndim],                       &
                            "nDindex%skip_li",name_sub,tab_lb=[0,1])
          nDindex%skip_li(:,:) = .FALSE.

          IF (present(skip_li)) THEN
            IF (any(shape(nDindex%skip_li) /= shape(skip_li))) THEN
              write(out_unit,*) ' ERROR in ',name_sub
              write(out_unit,*) '  nDindex%skip_li and skip_li have different shape.'
              write(out_unit,*) '          shape(skip_li)',shape(skip_li)
              write(out_unit,*) '  shape(nDindex%skip_li)',shape(nDindex%skip_li)
              write(out_unit,*) ' Check the fortran source!!'
              STOP
            END IF
            nDindex%skip_li = skip_li
          END IF
        END IF

        IF (present(tab_i_TO_l)) THEN
          CALL alloc_array(nDindex%Tab_i_TO_L,[nDindex%ndim],       &
                          "nDindex%Tab_i_TO_L",name_sub)
          DO i=1,nDindex%ndim
            nDindex%Tab_i_TO_L(i) = Tab_i_TO_L(i)
          END DO
        END IF


        IF (present(nDsize)) THEN
          nDindex%nDsize(:) = nDsize(:)
        ELSE
          nDindex%nDsize(:) = -1
        END IF

        IF (present(nDweight)) THEN
          nDindex%nDweight(:) = nDweight(:)
        END IF

        IF (present(nDinit)) THEN
           nDindex%nDinit(:) = nDinit(:)
        END IF

        IF (present(nDend)) THEN
          nDindex%nDend(:) = nDend(:)
        ELSE
          nDindex%nDend(:) = -1
        END IF

        IF (present(nDend) .AND. present(nDsize)) THEN
          write(out_unit,*) ' ERROR in ',name_sub
          write(out_unit,*) '  nDend(:) and nDsize(:) are both present.'
          write(out_unit,*) '   It is not possible'
          write(out_unit,*) ' Check the fortran source!!'
          STOP 'ERROR in init_nDindexPrim : nDend(:) and nDsize(:) are both present'
        ELSE IF (.NOT. present(nDend) .AND. present(nDsize)) THEN
          nDindex%nDend(:) = nDindex%nDsize(:) + nDindex%nDinit(:) - 1
        ELSE IF (present(nDend) .AND. .NOT. present(nDsize)) THEN
          nDindex%nDsize(:) = nDindex%nDend(:) - nDindex%nDinit(:) + 1
        ELSE
          write(out_unit,*) ' ERROR in ',name_sub
          write(out_unit,*) '  nDend(:) and nDsize(:) are both absent.'
          write(out_unit,*) '   It is not possible'
          write(out_unit,*) ' Check the fortran source!!'
          STOP 'ERROR in init_nDindexPrim : nDend(:) and nDsize(:) are both absent'
        END IF

       IF (present(type_OF_nDindex)) THEN
         nDindex%type_OF_nDindex = type_OF_nDindex
       ELSE
         IF (nDindex%type_OF_nDindex == -1) THEN
           nDindex%type_OF_nDindex = 1
         END IF
       END IF

        IF (present(MaxCoupling)) THEN
          nDindex%MaxCoupling = MaxCoupling
        ELSE
          nDindex%MaxCoupling = ndim
        END IF
        IF (nDindex%MaxCoupling < 1) THEN
          write(out_unit,*) ' ERROR in ',name_sub
          write(out_unit,*) ' MaxCoupling is < 1 !!',nDindex%MaxCoupling
          write(out_unit,*) ' Check the fortran source!!'
          STOP
        END IF
        IF (present(MinCoupling)) THEN
          nDindex%MinCoupling = MinCoupling
        ELSE
          nDindex%MinCoupling = 0
        END IF
        IF (nDindex%MinCoupling > ndim+1) THEN
          write(out_unit,*) ' ERROR in ',name_sub
          write(out_unit,*) ' MinCoupling is < ndim+1 !!',nDindex%MinCoupling
          write(out_unit,*) ' MinCoupling,ndim',nDindex%MinCoupling,ndim

          write(out_unit,*) ' Check the fortran source!!'
          STOP
        END IF

        IF (nDindex%With_L) THEN
             nDindex%Lmax    = Lmax
             nDindex%MaxNorm = real(Lmax,kind=rkind)
           IF (present(Lmin)) THEN
             nDindex%Lmin    = Lmin
             nDindex%MinNorm = real(Lmin,kind=rkind)
           ELSE
             nDindex%Lmin    = 0
             nDindex%MinNorm = ZERO
           END IF

           IF (present(L1max)) THEN
             nDindex%L1max    = L1max
           ELSE
             nDindex%L1max    = Lmax
           END IF

           IF (present(L2max)) THEN
             nDindex%L2max    = L2max
           ELSE
             nDindex%L2max    = Lmax
           END IF

        ELSE
           IF (present(MinNorm)) THEN
             nDindex%MinNorm = MinNorm
           ELSE
             nDindex%MinNorm = ZERO
           END IF

           IF (present(MaxNorm)) THEN
             nDindex%MaxNorm = MaxNorm
           ELSE
             IF (nDindex%MaxNorm < ZERO) THEN
               nDindex%MaxNorm = calc_Norm_OF_nDval(nDindex%nDend,nDindex)
             END If
           END IF
        END IF

      END IF

      ! with the full initialization.
       IF (nDindex%init .AND. With_nDindex_loc) THEN

         SELECT CASE (nDindex%type_OF_nDindex)
         CASE (0)
           CALL init_nDindex_type0(nDindex,err_sub_loc)

         CASE (1,-1)
           CALL init_nDindex_type1(nDindex,err_sub_loc)

         CASE (2)
           CALL init_nDindex_type2(nDindex,err_sub_loc)

         CASE (3)
           CALL init_nDindex_type3(nDindex,order01=.TRUE.,err_sub=err_sub_loc)
         CASE (-3)
           CALL init_nDindex_type3(nDindex,order01=.FALSE.,err_sub=err_sub_loc)

         CASE (4)
           STOP ' STOP in init_nDindexPrim, type_OF_nDindex=4 not tested'
           !CALL init_nDindex_type4(nDindex,order01=.TRUE.,err_sub=err_sub_loc)
         CASE (-4)
           CALL init_nDindex_type4(nDindex,order01=.FALSE.,err_sub=err_sub_loc)

         CASE (5)
           CALL init_nDindex_type5p(nDindex,err_sub=err_sub_loc)
         CASE (-5)
           CALL init_nDindex_type5m(nDindex,err_sub=err_sub_loc)

         CASE DEFAULT
           write(out_unit,*) ' ERROR in ',name_sub
           write(out_unit,*) 'type_OF_nDindex',nDindex%type_OF_nDindex
           STOP
         END SELECT

         IF (nDindex%packed) CALL pack_nDindex(nDindex)

       END IF

       IF (present(err_sub)) THEN
         err_sub = err_sub_loc
       ELSE
         IF (err_sub_loc /= 0) STOP
       END IF

!-----------------------------------------------------------
      IF (debug) THEN
        CALL Write_nDindex(nDindex)
        write(out_unit,*) 'END ',name_sub
        flush(out_unit)
      END IF
!-----------------------------------------------------------

      END SUBROUTINE init_nDindexPrim
!     ==================================================================
!     initialization of nDindex of type 0
!     ==================================================================
      !!@description: TODO
      !!@param: TODO
      SUBROUTINE init_nDindex_type2(nDindex,err_sub)

        TYPE (Type_nDindex),  intent(inout)           :: nDindex
        integer,              intent(inout)           :: err_sub

        logical :: test
        integer :: i,nDI,nb_Coupling,nDval(nDindex%ndim)
        real (kind=Rkind) :: Norm

!----- for debuging --------------------------------------------------
      integer :: err_mem,memory
      character (len=*), parameter :: name_sub='init_nDindex_type2'
      logical,parameter :: debug=.FALSE.
!      logical,parameter :: debug=.TRUE.
!-----------------------------------------------------------
      IF (debug) THEN
        write(out_unit,*) 'BEGINNING ',name_sub
      END IF
!-----------------------------------------------------------
      err_sub = 0

        IF (.NOT. nDindex%init) THEN
          write(out_unit,*) ' ERROR in ',name_sub
          write(out_unit,*) ' nDindex has to be initialized!!'
          write(out_unit,*) ' CHECK the fortran source!!'
          STOP
        END IF

        IF (nDindex%type_OF_nDindex /= 2) THEN
          write(out_unit,*) ' ERROR in ',name_sub
          write(out_unit,*) ' type_OF_nDindex MUST be set to 2'
          write(out_unit,*) ' type_OF_nDindex:',nDindex%type_OF_nDindex
          write(out_unit,*) ' CHECK the fortran source!!'
          STOP
        END IF

        nDindex%packed = .TRUE. ! should be remove ...

        ! first the number of points
        CALL calc_Max_nDI_type2(nDindex)

        IF (nDindex%Write_Tab .OR. debug)                               &
                      write(out_unit,*) 'nDindex%Max_nDI',nDindex%Max_nDI

        IF (nDindex%Max_nDI <= 0) THEN
          write(out_unit,*) ' ERROR in ',name_sub
          write(out_unit,*) ' Max_nDI MUST be set > 0'
          write(out_unit,*) ' Max_nDI:',nDindex%Max_nDI
          err_sub = err_Max_nDI
          RETURN
        END IF

        ! Then the table of nDval and Norm: Tab_nDval, Tab_Norm
        IF (nDindex%packed) THEN
          CALL alloc_NParray(nDindex%Tab_nDval,                           &
                                     [nDindex%ndim,nDindex%Max_nDI],  &
                           "nDindex%Tab_nDval",name_sub)
          CALL alloc_NParray(nDindex%Tab_Norm,[nDindex%Max_nDI],        &
                          "nDindex%Tab_Norm",name_sub)
          CALL alloc_NParray(nDindex%Tab_L,[nDindex%Max_nDI],         &
                            "nDindex%Tab_L",name_sub)
        END IF

        nDI = 0
        nDval(:) = nDindex%nDinit(:)
        nDval(nDindex%ndim) = nDval(nDindex%ndim) - 1
        DO
          nDval(nDindex%ndim) = nDval(nDindex%ndim) + 1
          DO i=nDindex%ndim,1,-1
            test = .TRUE.
            IF (nDval(i) <= nDindex%nDend(i)) THEN
              Norm = calc_Norm_OF_nDval(nDval,nDindex)
              nb_Coupling = count((nDval-nDindex%nDinit) > 0)
              test = Norm > nDindex%MaxNorm .OR.                        &
                     Norm < nDindex%MinNorm .OR.                        &
                     nb_Coupling > nDindex%MaxCoupling .OR.             &
                     nb_Coupling < nDindex%MinCoupling
            END IF

            IF (test) THEN
              nDval(i) = nDindex%nDinit(i)
              IF (i>1) nDval(i-1) = nDval(i-1) + 1
            ELSE
              nDI = nDI + 1
              EXIT
            END IF
          END DO
          IF (test) EXIT
          IF (nDindex%packed) THEN
            IF (Norm >= nDindex%MinNorm) THEN
              nDindex%Tab_nDval(:,nDI) = nDval(:)
              nDindex%Tab_Norm(nDI)    = norm
              nDindex%Tab_L(nDI)       = int(norm)
            END IF
          END IF
          !IF (nDindex%Write_Tab .OR. debug)                             &
             !write(out_unit,*) 'nDI,nDval',nDI,':',nDval,' Norm:',Norm
        END DO
        IF (nDI /= nDindex%Max_nDI) THEN
          write(out_unit,*) ' ERROR in ',name_sub
          write(out_unit,*) ' nDI MUST be equal to Max_nDI'
          write(out_unit,*) ' nDI,Max_nDI:',nDI,nDindex%Max_nDI
          write(out_unit,*) ' Check the fortran source!'
          err_sub = err_nDI
          RETURN
        END IF

        nDindex%packed_done = nDindex%packed
!-----------------------------------------------------------
      IF (debug) THEN
        CALL Write_nDindex(nDindex)
        write(out_unit,*) 'END ',name_sub
      END IF
!-----------------------------------------------------------

      END SUBROUTINE init_nDindex_type2
      SUBROUTINE init_nDindex_type0(nDindex,err_sub)

        TYPE (Type_nDindex)        :: nDindex
        integer,              intent(inout)           :: err_sub

        logical :: test
        integer :: i,nDI,nb_Coupling,nDval(nDindex%ndim)
        real (kind=Rkind) :: Norm

!----- for debuging --------------------------------------------------
      integer :: err_mem,memory
      character (len=*), parameter :: name_sub='init_nDindex_type0'
      logical,parameter :: debug=.FALSE.
!      logical,parameter :: debug=.TRUE.
!-----------------------------------------------------------
      IF (debug) THEN
        write(out_unit,*) 'BEGINNING ',name_sub
      END IF
!-----------------------------------------------------------
      err_sub = 0

        IF (.NOT. nDindex%init) THEN
          write(out_unit,*) ' ERROR in ',name_sub
          write(out_unit,*) ' nDindex has to be initialized!!'
          write(out_unit,*) ' CHECK the fortran source!!'
          STOP
        END IF

        IF (nDindex%type_OF_nDindex /= 0) THEN
          write(out_unit,*) ' ERROR in ',name_sub
          write(out_unit,*) ' type_OF_nDindex MUST be set to 0'
          write(out_unit,*) ' type_OF_nDindex:',nDindex%type_OF_nDindex
          write(out_unit,*) ' CHECK the fortran source!!'
          STOP
        END IF

        nDindex%packed = .TRUE. ! should be remove ...

        ! first the number of points
        CALL calc_Max_nDI_type0(nDindex)

        IF (nDindex%Write_Tab .OR. debug)                               &
                      write(out_unit,*) 'nDindex%Max_nDI',nDindex%Max_nDI

        IF (nDindex%Max_nDI <= 0) THEN
          write(out_unit,*) ' ERROR in ',name_sub
          write(out_unit,*) ' Max_nDI MUST be set > 0'
          write(out_unit,*) ' Max_nDI:',nDindex%Max_nDI
          err_sub = err_Max_nDI
          RETURN
        END IF

        ! Then the table of nDval and Norm: Tab_nDval, Tab_Norm
        IF (nDindex%packed) THEN
          CALL alloc_NParray(nDindex%Tab_nDval,                           &
                                     [nDindex%ndim,nDindex%Max_nDI],  &
                          "nDindex%Tab_nDval",name_sub)
          CALL alloc_NParray(nDindex%Tab_Norm,[nDindex%Max_nDI],        &
                          "nDindex%Tab_Norm",name_sub)
          CALL alloc_NParray(nDindex%Tab_L,[nDindex%Max_nDI],         &
                            "nDindex%Tab_L",name_sub)
        END IF

        nDI = 0
        nDval(:) = nDindex%nDinit(:)
        nDval(nDindex%ndim) = nDval(nDindex%ndim) - 1
        DO
          nDval(nDindex%ndim) = nDval(nDindex%ndim) + 1
          DO i=nDindex%ndim,1,-1
            test = .TRUE.
            IF (nDval(i) <= nDindex%nDend(i)) THEN
              Norm = calc_Norm_OF_nDval(nDval,nDindex)
              test = Norm > nDindex%MaxNorm
            END IF

            IF (test) THEN
              nDval(i) = nDindex%nDinit(i)
              IF (i>1) nDval(i-1) = nDval(i-1) + 1
            ELSE
              nb_Coupling = count((nDval-nDindex%nDinit) > 0)
              !write(out_unit,*) 'nb_coupling',nDval,':',nb_Coupling
              IF (Norm >= nDindex%MinNorm .AND.                         &
                  nb_Coupling <= nDindex%MaxCoupling .AND.              &
                  nb_Coupling >= nDindex%MinCoupling) THEN
                 nDI = nDI + 1
              END IF
              EXIT
            END IF
          END DO
          IF (test) EXIT
          IF (nDindex%packed) THEN
            IF (Norm >= nDindex%MinNorm .AND.                           &
                nb_Coupling <= nDindex%MaxCoupling .AND.                &
                nb_Coupling >= nDindex%MinCoupling) THEN
              nDindex%Tab_nDval(:,nDI) = nDval(:)
              nDindex%Tab_Norm(nDI)    = norm
              nDindex%Tab_L(nDI)       = int(norm)
            END IF
          END IF
          !IF (nDindex%Write_Tab .OR. debug)                             &
             !write(out_unit,*) 'nDI,nDval',nDI,':',nDval,' Norm:',Norm
        END DO
        IF (nDI /= nDindex%Max_nDI) THEN
          write(out_unit,*) ' ERROR in ',name_sub
          write(out_unit,*) ' nDI MUST be equal to Max_nDI'
          write(out_unit,*) ' nDI,Max_nDI:',nDI,nDindex%Max_nDI
          err_sub = err_nDI
          RETURN
        END IF

        nDindex%packed_done = nDindex%packed
!-----------------------------------------------------------
      IF (debug) THEN
        CALL Write_nDindex(nDindex)
        write(out_unit,*) 'END ',name_sub
      END IF
!-----------------------------------------------------------

      END SUBROUTINE init_nDindex_type0
      !!@description: TODO
      !!@param: TODO
      SUBROUTINE init_nDindex_type1(nDindex,err_sub)

        TYPE (Type_nDindex)        :: nDindex
        integer,              intent(inout)           :: err_sub

        integer :: i,nDI,nDval(nDindex%ndim)
        real (kind=Rkind) :: Norm


!-----------------------------------------------------------
      integer :: err_mem,memory
      character (len=*), parameter :: name_sub='init_nDindex_type1'
      logical,parameter :: debug=.FALSE.
!      logical,parameter :: debug=.TRUE.
!-----------------------------------------------------------
      IF (debug) THEN
        write(out_unit,*) 'BEGINNING ',name_sub
      END IF
!-----------------------------------------------------------
      err_sub = 0

        IF (.NOT. nDindex%init) THEN
          write(out_unit,*) ' ERROR in ',name_sub
          write(out_unit,*) ' nDindex has to be initialized!!'
          write(out_unit,*) ' Check the fortran source!!'
          STOP
        END IF

        IF (abs(nDindex%type_OF_nDindex) /= 1) THEN
          write(out_unit,*) ' ERROR in ',name_sub
          write(out_unit,*) ' type_OF_nDindex MUST be set to 1 or -1'
          write(out_unit,*) ' type_OF_nDindex:',nDindex%type_OF_nDindex
          write(out_unit,*) ' Check the fortran source!!'
          STOP
        END IF

        ! first the number of points
        CALL calc_Max_nDI(nDindex)

        IF (nDindex%Write_Tab .OR. debug) write(out_unit,*) 'nDindex%Max_nDI',nDindex%Max_nDI

        ! Then the table of nDval: Tab_nDval
        IF (nDindex%packed) THEN
          CALL alloc_NParray(nDindex%Tab_nDval,                         &
                                     [nDindex%ndim,nDindex%Max_nDI],  &
                          "nDindex%Tab_nDval",name_sub)
          CALL alloc_NParray(nDindex%Tab_Norm,[nDindex%Max_nDI],      &
                          "nDindex%Tab_Norm",name_sub)
          CALL alloc_NParray(nDindex%Tab_L,[nDindex%Max_nDI],         &
                            "nDindex%Tab_L",name_sub)
          nDindex%Tab_nDval(:,:) = 0
          nDindex%Tab_Norm(:)    = ZERO
          nDindex%Tab_L(:)       = 0

          DO nDI=1,nDindex%Max_nDI
            CALL calc_nDindex(nDindex,nDI,nDval)
            Norm = calc_Norm_OF_nDval(nDval,nDindex)
            IF (nDindex%packed) THEN
              nDindex%Tab_nDval(:,nDI) = nDval(:)
              nDindex%Tab_Norm(nDI)    = Norm
              nDindex%Tab_L(nDI)       = int(Norm)
            END IF
            IF (nDindex%Write_Tab  .OR. debug)                          &
               write(out_unit,*) 'nDI,nDval',nDI,':',nDval,            &
                                  ' Norm:',Norm,' L',nDindex%Tab_L(nDI)
          END DO

          nDindex%packed_done = nDindex%packed
        END IF

!-----------------------------------------------------------
      IF (debug) THEN
        CALL Write_nDindex(nDindex)
        write(out_unit,*) 'END ',name_sub
      END IF
!-----------------------------------------------------------

      END SUBROUTINE init_nDindex_type1

      SUBROUTINE init_nDindex_typeTAB(nDindex,ndim,Tab_nDval,Max_nDI,err_sub)

        TYPE (Type_nDindex), intent(inout) :: nDindex
        integer,              intent(inout)           :: err_sub

        integer, intent(in) :: ndim,Max_nDI
        integer, intent(in) :: Tab_nDval(:,:)

        integer :: nDI

!----- for debuging --------------------------------------------------
      integer :: err_mem,memory
      character (len=*), parameter :: name_sub='init_nDindex_typeTAB'
      logical,parameter :: debug=.FALSE.
!      logical,parameter :: debug=.TRUE.
!-----------------------------------------------------------
      IF (debug) THEN
        write(out_unit,*) 'BEGINNING ',name_sub
        write(out_unit,*) 'ndim,Max_nDI',ndim,Max_nDI

      END IF
!-----------------------------------------------------------
      err_sub = 0

        IF (nDindex%init) THEN
          write(out_unit,*) ' ERROR in ',name_sub
          write(out_unit,*) ' nDindex is already initialized!!'
          write(out_unit,*) ' Check the fortran source!!'
          STOP
        END IF

        CALL alloc_nDindex(nDindex,ndim)

        nDindex%init            = .TRUE.
        nDindex%packed          = .TRUE.
        nDindex%type_OF_nDindex = 0 ! it should be changed!!

        nDindex%ndim            = ndim
        nDindex%Max_nDI         = Max_nDI


        nDindex%MinNorm         = ZERO
        nDindex%MaxNorm         = ZERO


        CALL alloc_NParray(nDindex%Tab_nDval,                           &
                                     [nDindex%ndim,nDindex%Max_nDI],  &
                          "nDindex%Tab_nDval",name_sub)
        nDindex%Tab_nDval(:,:) = Tab_nDval(:,:)

        CALL alloc_NParray(nDindex%Tab_L,[nDindex%Max_nDI],           &
                          "nDindex%Tab_L",name_sub)

        IF (allocated(nDindex%Tab_Norm)) THEN
          CALL dealloc_NParray(nDindex%Tab_Norm,"nDindex%Tab_Norm",name_sub)
        END IF
        CALL alloc_NParray(nDindex%Tab_Norm,[nDindex%Max_nDI],        &
                          "nDindex%Tab_Norm",name_sub)


        DO nDI=1,nDindex%Max_nDI
          nDindex%Tab_Norm(nDI)    = calc_Norm_OF_nDval(Tab_nDval(:,nDI),nDindex)
        END DO
        nDindex%Tab_L(:) = int(nDindex%Tab_Norm)
        nDindex%packed_done = nDindex%packed

!-----------------------------------------------------------
      IF (debug) THEN
        CALL Write_nDindex(nDindex)
        write(out_unit,*) 'END ',name_sub
      END IF
!-----------------------------------------------------------

      END SUBROUTINE init_nDindex_typeTAB

      SUBROUTINE init_nDindex_type3(nDindex,order01,err_sub)

        TYPE (Type_nDindex)        :: nDindex
        logical                    :: order01
        integer,              intent(inout)           :: err_sub

        integer :: id,Lmin,Lmax

!----- for debuging --------------------------------------------------
      integer :: err_mem,memory
      character (len=*), parameter :: name_sub='init_nDindex_type3'
      logical,parameter :: debug=.FALSE.
      !logical,parameter :: debug=.TRUE.
!-----------------------------------------------------------
      IF (debug) THEN
        write(out_unit,*) 'BEGINNING ',name_sub
        write(out_unit,*) 'order01 ',order01
      END IF
!-----------------------------------------------------------
      err_sub = 0

        IF (.NOT. nDindex%init) THEN
          write(out_unit,*) ' ERROR in ',name_sub
          write(out_unit,*) ' nDindex has to be initialized!!'
          write(out_unit,*) ' CHECK the fortran source!!'
          STOP
        END IF

        IF (abs(nDindex%type_OF_nDindex) /= 3) THEN
          write(out_unit,*) ' ERROR in ',name_sub
          write(out_unit,*) ' type_OF_nDindex MUST be set to 3 or -3'
          write(out_unit,*) ' type_OF_nDindex:',nDindex%type_OF_nDindex
          write(out_unit,*) ' CHECK the fortran source!!'
          STOP
        END IF

        IF (.NOT. allocated(nDindex%tab_i_TO_l) .AND. nDindex%type_OF_nDindex == 3) THEN
          write(out_unit,*) ' ERROR in ',name_sub
          write(out_unit,*) ' The tab_i_TO_l must be allocated'
          write(out_unit,*) '   with type_OF_nDindex=3'
          write(out_unit,*) ' CHECK the fortran source!!'
          STOP
        END IF

        IF (.NOT. nDindex%With_L) THEN
          write(out_unit,*) ' ERROR in ',name_sub
          write(out_unit,*) ' You should use L instead of Norm'
          write(out_unit,*) '   with type_OF_nDindex=3'
          write(out_unit,*) ' CHECK the fortran source!!'
          STOP
        END IF

        nDindex%packed = .TRUE. ! should be remove ...

        Lmin = nDindex%Lmin
        Lmax = nDindex%Lmax

        IF (order01) THEN
          CALL Set_nDInd_01order(nDindex%Tab_DInd,nDindex%ndim,     &
                                 Lmin,Lmax,tab_i_TO_l=nDindex%tab_i_TO_l)
          id = nDindex%ndim+1
        ELSE
          CALL Set_nDInd_10order(nDindex%Tab_DInd,nDindex%ndim,     &
                                 Lmin,Lmax,tab_i_TO_l=nDindex%tab_i_TO_l)
          id = 0
        END IF
        !CALL Write_Tab_nDInd(nDindex%Tab_DInd)
        ! tranfer the nDindex%Tab_DInd to nDindex parameters

        nDindex%Max_nDI = nDindex%Tab_DInd(id)%MaxnD

        CALL alloc_NParray(nDindex%Tab_nDval,                             &
                                     [nDindex%ndim,nDindex%Max_nDI],    &
                        "nDindex%Tab_nDval",name_sub)
        CALL alloc_NParray(nDindex%Tab_Norm,[nDindex%Max_nDI],          &
                        "nDindex%Tab_Norm",name_sub)
        CALL alloc_NParray(nDindex%Tab_L,[nDindex%Max_nDI],             &
                          "nDindex%Tab_L",name_sub)

        nDindex%Tab_nDval(:,:) = nDindex%Tab_DInd(id)%tab_ind(:,:)
        nDindex%Tab_Norm(:)    = real(nDindex%Tab_DInd(id)%i_TO_l(:),kind=Rkind)
        nDindex%Tab_L(:)       = nDindex%Tab_DInd(id)%i_TO_l(:)

        IF (nDindex%Write_Tab .OR. debug)                               &
                      write(out_unit,*) 'nDindex%Max_nDI',nDindex%Max_nDI

        IF (nDindex%Max_nDI <= 0) THEN
          write(out_unit,*) ' ERROR in ',name_sub
          write(out_unit,*) ' Max_nDI MUST be set > 0'
          write(out_unit,*) ' Max_nDI:',nDindex%Max_nDI
          err_sub = err_Max_nDI
          RETURN
        END IF

        nDindex%packed_done = nDindex%packed
!-----------------------------------------------------------
      IF (debug) THEN
        CALL Write_nDindex(nDindex)
        write(out_unit,*) 'END ',name_sub
      END IF
!-----------------------------------------------------------

      END SUBROUTINE init_nDindex_type3

      ! same as init_nDindex_type3 but without nDindex%tab_i_TO_l
      ! tab in l
      SUBROUTINE init_nDindex_type4(nDindex,order01,err_sub)

        TYPE (Type_nDindex)        :: nDindex
        logical                    :: order01
        integer,              intent(inout)           :: err_sub

        integer :: id,Lmin,Lmax

!----- for debuging --------------------------------------------------
      integer :: err_mem,memory
      character (len=*), parameter :: name_sub='init_nDindex_type4'
      logical,parameter :: debug=.FALSE.
      !logical,parameter :: debug=.TRUE.
!-----------------------------------------------------------
      IF (debug) THEN
        write(out_unit,*) 'BEGINNING ',name_sub
        write(out_unit,*) 'order01 ',order01
      END IF
!-----------------------------------------------------------
      err_sub = 0

        IF (.NOT. nDindex%init) THEN
          write(out_unit,*) ' ERROR in ',name_sub
          write(out_unit,*) ' nDindex has to be initialized!!'
          write(out_unit,*) ' CHECK the fortran source!!'
          STOP
        END IF

        IF (abs(nDindex%type_OF_nDindex) /= 4) THEN
          write(out_unit,*) ' ERROR in ',name_sub
          write(out_unit,*) ' type_OF_nDindex MUST be set to 4 or -4'
          write(out_unit,*) ' type_OF_nDindex:',nDindex%type_OF_nDindex
          write(out_unit,*) ' CHECK the fortran source!!'
          STOP
        END IF

        IF (.NOT. nDindex%With_L) THEN
          write(out_unit,*) ' ERROR in ',name_sub
          write(out_unit,*) ' You should use L instead of Norm'
          write(out_unit,*) '   with type_OF_nDindex=3'
          write(out_unit,*) ' CHECK the fortran source!!'
          STOP
        END IF

        nDindex%packed = .TRUE. ! should be remove ...

        Lmin = nDindex%Lmin
        Lmax = nDindex%Lmax
        IF (order01) THEN
          CALL Set_nDInd_01order_L(nDindex%Tab_DInd,nDindex%ndim,Lmin,Lmax)
          id = nDindex%ndim+1
        ELSE
          CALL Set_nDInd_10order_L(nDindex%Tab_DInd,nDindex%ndim,Lmin,Lmax)
          id = 0
        END IF
        !CALL Write_Tab_nDInd(nDindex%Tab_DInd)
        ! tranfer the nDindex%Tab_DInd to nDindex parameters

        nDindex%Max_nDI = nDindex%Tab_DInd(id)%MaxnD

        CALL alloc_NParray(nDindex%Tab_nDval,                             &
                                     [nDindex%ndim,nDindex%Max_nDI],  &
                        "nDindex%Tab_nDval",name_sub)
        CALL alloc_NParray(nDindex%Tab_Norm,[nDindex%Max_nDI],          &
                        "nDindex%Tab_Norm",name_sub)
        CALL alloc_NParray(nDindex%Tab_L,[nDindex%Max_nDI],           &
                          "nDindex%Tab_L",name_sub)

        nDindex%Tab_nDval(:,:) = nDindex%Tab_DInd(id)%tab_ind(:,:)
        nDindex%Tab_Norm(:)    = real(nDindex%Tab_DInd(id)%i_TO_l(:),kind=Rkind)
        nDindex%Tab_L(:)       = nDindex%Tab_DInd(id)%i_TO_l(:)

        IF (nDindex%Write_Tab .OR. debug)                               &
                      write(out_unit,*) 'nDindex%Max_nDI',nDindex%Max_nDI

        IF (nDindex%Max_nDI <= 0) THEN
          write(out_unit,*) ' ERROR in ',name_sub
          write(out_unit,*) ' Max_nDI MUST be set > 0'
          write(out_unit,*) ' Max_nDI:',nDindex%Max_nDI
          err_sub = err_Max_nDI
        END IF

        nDindex%packed_done = nDindex%packed
!-----------------------------------------------------------
      IF (debug) THEN
        CALL Write_nDindex(nDindex)
        write(out_unit,*) 'END ',name_sub
      END IF
!-----------------------------------------------------------

      END SUBROUTINE init_nDindex_type4

  SUBROUTINE init_nDindex_type5p(nDindex,err_sub)

  TYPE (Type_nDindex),  intent(inout)           :: nDindex
  integer,              intent(inout)           :: err_sub

  logical :: In_the_list
  integer :: i,L,L1,L2,nDI,nb_Coupling,nDval(nDindex%ndim)
  integer :: iiG,iG,maxth,ith

  real (kind=Rkind) :: Norm

!----- for debuging --------------------------------------------------
  integer :: err_mem,memory
  character (len=*), parameter :: name_sub='init_nDindex_type5p'
  logical,parameter :: debug=.FALSE.
  !logical,parameter :: debug=.TRUE.
!-----------------------------------------------------------
  IF (debug) THEN
    write(out_unit,*) 'BEGINNING ',name_sub
    write(out_unit,*) 'Lmax,L1max,L2max',nDindex%Lmax,nDindex%L1max,nDindex%L2max
    write(out_unit,*) 'nDNum_OF_Lmax',nDindex%nDNum_OF_Lmax
    !CALL Write_nDindex(nDindex)
    flush(out_unit)
  END IF
!-----------------------------------------------------------
  err_sub = 0

  IF (.NOT. nDindex%init) THEN
    write(out_unit,*) ' ERROR in ',name_sub
    write(out_unit,*) ' nDindex has to be initialized!!'
    write(out_unit,*) ' CHECK the fortran source!!'
    STOP
  END IF
  IF (nDindex%type_OF_nDindex /= 5) THEN
    write(out_unit,*) ' ERROR in ',name_sub
    write(out_unit,*) ' type_OF_nDindex MUST be set to 5'
    write(out_unit,*) ' type_OF_nDindex:',nDindex%type_OF_nDindex
    write(out_unit,*) ' CHECK the fortran source!!'
    STOP
  END IF

  ! first the number of points
  CALL calc_Max_nDI_type5p(nDindex)

  IF (nDindex%Write_Tab .OR. debug) THEN
    write(out_unit,*) 'nDindex%Max_nDI',nDindex%Max_nDI
    flush(out_unit)
  END IF

  IF (nDindex%Max_nDI <= 0) THEN
    write(out_unit,*) ' ERROR in ',name_sub
    write(out_unit,*) ' Max_nDI MUST be set > 0'
    write(out_unit,*) ' Max_nDI:',nDindex%Max_nDI
    err_sub = err_Max_nDI
    RETURN
  END IF

  IF (nDindex%packed) THEN
    CALL alloc_NParray(nDindex%Tab_nDval,                         &
                               [nDindex%ndim,nDindex%Max_nDI],  &
                      "nDindex%Tab_nDval",name_sub)
    CALL alloc_NParray(nDindex%Tab_Norm,[nDindex%Max_nDI],      &
                      "nDindex%Tab_Norm",name_sub)
    CALL alloc_NParray(nDindex%Tab_L,[nDindex%Max_nDI],         &
                      "nDindex%Tab_L",name_sub)
  END IF


  nDI = 0
  nDval(:) = nDindex%nDinit(:)
  nDval(nDindex%ndim) = nDval(nDindex%ndim) - 1

  DO
    CALL ADD_ONE_TO_nDindex_type5p(nDval,nDindex,In_the_list)

    IF (.NOT. In_the_list) EXIT
    nDI = nDI + 1

    IF (nDindex%packed) THEN
      CALL calc_LL1L2_OF_nDindex_type5(L,L1,L2,nDval,nDindex)
      nDindex%Tab_nDval(:,nDI) = nDval(:)
      nDindex%Tab_Norm(nDI)    = real(L,kind=Rkind)
      nDindex%Tab_L(nDI)       = L
    END IF
    IF (nDindex%Write_Tab .OR. debug)  THEN
       IF (nDI < 100) THEN
         write(out_unit,*) 'nDI,nDval',nDI,':',nDval,' L:',L
       ELSE IF (nDI == 100) THEN
         write(out_unit,*) 'nDI,nDval ....'
       END IF
       flush(out_unit)
    END IF
  END DO
  IF (nDI /= nDindex%Max_nDI) THEN
    write(out_unit,*) ' ERROR in ',name_sub
    write(out_unit,*) ' nDI MUST be equal to Max_nDI'
    write(out_unit,*) ' nDI,Max_nDI:',nDI,nDindex%Max_nDI
    write(out_unit,*) ' Check the fortran source!'
    err_sub = err_nDI
    RETURN
  END IF

  nDindex%packed_done = nDindex%packed
!-----------------------------------------------------------
  IF (debug) THEN
    !CALL Write_nDindex(nDindex)
    write(out_unit,*) 'END ',name_sub
    flush(out_unit)
  END IF
!-----------------------------------------------------------

END SUBROUTINE init_nDindex_type5p
      SUBROUTINE init_nDindex_type5m(nDindex,err_sub)

        TYPE (Type_nDindex),  intent(inout)           :: nDindex
        integer,              intent(inout)           :: err_sub

        logical :: In_the_list
        integer :: i,L,L1,L2,nDI,nb_Coupling,nDval(nDindex%ndim)
        integer :: iiG,iG

        real (kind=Rkind) :: Norm

!----- for debuging --------------------------------------------------
      integer :: err_mem,memory
      character (len=*), parameter :: name_sub='init_nDindex_type5m'
      logical,parameter :: debug=.FALSE.
      !logical,parameter :: debug=.TRUE.
!-----------------------------------------------------------
      IF (debug) THEN
        write(out_unit,*) 'BEGINNING ',name_sub
        write(out_unit,*) 'Lmax,L1max,L2max',nDindex%Lmax,nDindex%L1max,nDindex%L2max
        write(out_unit,*) 'nDNum_OF_Lmax',nDindex%nDNum_OF_Lmax
        !CALL Write_nDindex(nDindex)
      END IF
!-----------------------------------------------------------
      err_sub = 0

        IF (.NOT. nDindex%init) THEN
          write(out_unit,*) ' ERROR in ',name_sub
          write(out_unit,*) ' nDindex has to be initialized!!'
          write(out_unit,*) ' CHECK the fortran source!!'
          STOP
        END IF

        IF (nDindex%type_OF_nDindex /= -5) THEN
          write(out_unit,*) ' ERROR in ',name_sub
          write(out_unit,*) ' type_OF_nDindex MUST be set to -5'
          write(out_unit,*) ' type_OF_nDindex:',nDindex%type_OF_nDindex
          write(out_unit,*) ' CHECK the fortran source!!'
          STOP
        END IF

        ! first the number of points
        CALL calc_Max_nDI_type5m(nDindex)

        IF (nDindex%Write_Tab .OR. debug)                               &
                      write(out_unit,*) 'nDindex%Max_nDI',nDindex%Max_nDI

        IF (nDindex%Max_nDI <= 0) THEN
          write(out_unit,*) ' ERROR in ',name_sub
          write(out_unit,*) ' Max_nDI MUST be set > 0'
          write(out_unit,*) ' Max_nDI:',nDindex%Max_nDI
          err_sub = err_Max_nDI
          RETURN
        END IF

        IF (nDindex%packed) THEN
          CALL alloc_NParray(nDindex%Tab_nDval,                         &
                                     [nDindex%ndim,nDindex%Max_nDI],  &
                            "nDindex%Tab_nDval",name_sub)
          CALL alloc_NParray(nDindex%Tab_Norm,[nDindex%Max_nDI],      &
                            "nDindex%Tab_Norm",name_sub)
          CALL alloc_NParray(nDindex%Tab_L,[nDindex%Max_nDI],         &
                            "nDindex%Tab_L",name_sub)
        END IF


        nDI = 0
        nDval(:) = nDindex%nDinit(:)
        nDval(1) = nDval(1) - 1

        DO
          CALL ADD_ONE_TO_nDindex_type5m(nDval,nDindex,In_the_list)

          IF (.NOT. In_the_list) EXIT
          nDI = nDI + 1

          IF (nDindex%packed) THEN
            CALL calc_LL1L2_OF_nDindex_type5(L,L1,L2,nDval,nDindex)
            nDindex%Tab_nDval(:,nDI) = nDval(:)
            nDindex%Tab_Norm(nDI)    = real(L,kind=Rkind)
            nDindex%Tab_L(nDI)       = L
          END IF
          IF (nDindex%Write_Tab .OR. debug)                             &
             write(out_unit,*) 'nDI,nDval',nDI,':',nDval,' L:',L
        END DO
        IF (nDI /= nDindex%Max_nDI) THEN
          write(out_unit,*) ' ERROR in ',name_sub
          write(out_unit,*) ' nDI MUST be equal to Max_nDI'
          write(out_unit,*) ' nDI,Max_nDI:',nDI,nDindex%Max_nDI
          write(out_unit,*) ' Check the fortran source!'
          err_sub = err_nDI
          RETURN
        END IF

        nDindex%packed_done = nDindex%packed
!-----------------------------------------------------------
      IF (debug) THEN
        !CALL Write_nDindex(nDindex)
        write(out_unit,*) 'END ',name_sub
      END IF
!-----------------------------------------------------------

      END SUBROUTINE init_nDindex_type5m
      SUBROUTINE BubbleSort_tab_Sort_nDI(tab_Sort_nDI,nDindex,ibasis)
        TYPE (Type_nDindex), intent (in) :: nDindex
        integer, intent (in)             :: ibasis
        integer, intent (inout)          :: tab_Sort_nDI(:)

        integer :: nDval1(nDindex%ndim),nDval2(nDindex%ndim)

        integer :: nDI0,nDI1,nDI2


          DO nDI1=1,nDindex%Max_nDI
            CALL calc_nDindex(nDindex,tab_Sort_nDI(nDI1),nDval1)
            nDval1(ibasis) = 0

            DO nDI2=nDI1+1,nDindex%Max_nDI
              CALL calc_nDindex(nDindex,tab_Sort_nDI(nDI2),nDval2)
              nDval2(ibasis) = 0

              IF (inferior_tab(nDval2,nDval1)) THEN
                nDI0               = tab_Sort_nDI(nDI1)
                tab_Sort_nDI(nDI1) = tab_Sort_nDI(nDI2)
                tab_Sort_nDI(nDI2) = nDI0
                nDval1(:) = nDval2(:)
              END IF
            END DO
          END DO

      END SUBROUTINE BubbleSort_tab_Sort_nDI

      RECURSIVE SUBROUTINE QSort_tab_Sort_nDI(tab_Sort_nDI,ifirst,ilast,nDindex,ibasis)
        TYPE (Type_nDindex), intent (in) :: nDindex
        integer, intent (in)             :: ibasis,ifirst,ilast
        integer, intent (inout)          :: tab_Sort_nDI(:)

        integer :: ipivot
        real(kind=Rkind) :: r

        IF (ifirst < ilast) THEN
          CALL RANDOM_NUMBER(r)
          ipivot = ifirst + int(r*real(ilast-ifirst,kind=Rkind))! pivot
          ipivot = min(ipivot,ilast)
          ipivot = max(ipivot,ifirst)

          CALL Partition_tab_Sort_nDI(tab_Sort_nDI,ifirst,ilast,ipivot,nDindex,ibasis)
          CALL QSort_tab_Sort_nDI(tab_Sort_nDI,ifirst,ipivot-1,nDindex,ibasis)
          CALL QSort_tab_Sort_nDI(tab_Sort_nDI,ipivot+1,ilast,nDindex,ibasis)
        END IF

      END SUBROUTINE QSort_tab_Sort_nDI
      SUBROUTINE Partition_tab_Sort_nDI(tab_Sort_nDI,ifirst,ilast,ipivot,nDindex,ibasis)
        TYPE (Type_nDindex), intent (in) :: nDindex
        integer, intent (in)             :: ibasis,ifirst,ilast
        integer, intent (inout)          :: tab_Sort_nDI(:)
        integer, intent(inout) :: ipivot

        integer :: i, j,itemp
        integer :: nDval0(nDindex%ndim),nDvali(nDindex%ndim),nDvalj(nDindex%ndim)
        integer :: nDval_temp(nDindex%ndim)


        !write(out_unit,*) 'ifirst,ilast,ipivot',ifirst,ilast,ipivot


        CALL calc_nDindex(nDindex,tab_Sort_nDI(ipivot),nDval0)
        nDval0(ibasis) = 0
        !write(out_unit,*) 'nDval0',nDval0
        !permutation ipivot <=> ilast (nDval0 ne change pas)
        itemp                = tab_Sort_nDI(ipivot)
        tab_Sort_nDI(ipivot) = tab_Sort_nDI(ilast)
        tab_Sort_nDI(ilast)  = itemp


        j = ifirst
        DO i=ifirst,ilast-1
          CALL calc_nDindex(nDindex,tab_Sort_nDI(i),nDvali)
          nDvali(ibasis) = 0
          !write(out_unit,*) 'nDvali',nDvali
          !write(out_unit,*) 'j,i',j,i

          IF (inferior_tab(nDvali,nDval0)) THEN
            !write(out_unit,*) 'perm i,j',i,j

            itemp           = tab_Sort_nDI(i)
            tab_Sort_nDI(i) = tab_Sort_nDI(j)
            tab_Sort_nDI(j) = itemp

            j = j+1
          END IF

        END DO
        !write(out_unit,*) 'j pivot ?',j

        itemp               = tab_Sort_nDI(ilast)
        tab_Sort_nDI(ilast) = tab_Sort_nDI(j)
        tab_Sort_nDI(j)     = itemp

        ipivot = j


      END SUBROUTINE Partition_tab_Sort_nDI

      SUBROUTINE alloc_nDindex(nDindex,ndim)

        TYPE (Type_nDindex)        :: nDindex
        integer, intent(in)        :: ndim

        integer                    :: i,maxth
        integer :: err_mem,memory

        IF (nDindex%init) THEN
          write(out_unit,*) ' ERROR in alloc_nDindex'
          write(out_unit,*) ' nDindex is already initialized!!'
          write(out_unit,*) ' CHECK the fortran source!!'
          STOP
        END IF

        IF (ndim <= 0) THEN
          !write(out_unit,*) ' WARNING in alloc_nDindex'
          !write(out_unit,*) ' ndim <= 0'
          RETURN
        END IF

        IF (nDindex%alloc .AND. nDindex%ndim == ndim) THEN
          !write(out_unit,*) ' WARNNING in alloc_nDindex'
          !write(out_unit,*) ' alloc=t and nDindex%ndim == ndim',ndim
          !flush(out_unit)
          RETURN
        END IF
        nDindex%alloc = .TRUE.
        nDindex%ndim  = ndim

        IF (allocated(nDindex%nDsize))    THEN
          CALL dealloc_NParray(nDindex%nDsize,"nDindex%nDsize","alloc_nDindex")
        END IF
        CALL alloc_NParray(nDindex%nDsize,[ndim],                       &
                          "nDindex%nDsize","alloc_nDindex")

        IF (allocated(nDindex%nDweight))  THEN
          CALL dealloc_NParray(nDindex%nDweight,"nDindex%nDweight","alloc_nDindex")
        END IF
        CALL alloc_NParray(nDindex%nDweight,[ndim],                     &
                        "nDindex%nDweight","alloc_nDindex")

        IF (allocated(nDindex%nDinit))  THEN
          CALL dealloc_NParray(nDindex%nDinit,"nDindex%nDinit","alloc_nDindex")
        END IF
        CALL alloc_NParray(nDindex%nDinit,[ndim],                       &
                        "nDindex%nDinit","alloc_nDindex")


        IF (allocated(nDindex%nDend))     THEN
          CALL dealloc_NParray(nDindex%nDend,"nDindex%nDend","alloc_nDindex")
        END IF
        CALL alloc_NParray(nDindex%nDend,[ndim],                        &
                        "nDindex%nDend","alloc_nDindex")

        nDindex%nDsize(:)       = 0
        nDindex%nDweight(:)     = ONE
        nDindex%nDinit(:)       = 1
        nDindex%nDend(:)        = -1


      END SUBROUTINE alloc_nDindex
!     ==============================================================
!     ==============================================================
      !!@description: TODO
      !!@param: TODO
      SUBROUTINE dealloc_nDindex(nDindex)
        TYPE (Type_nDindex) :: nDindex

        integer          :: i
        integer          :: type_OF_nDindex,MaxCoupling,MinCoupling
        real(kind=Rkind) :: MaxNorm,MinNorm
        integer          :: Lmin,Lmax
        logical          :: With_L

        integer :: err_mem,memory

        type_OF_nDindex = nDindex%type_OF_nDindex
        MaxCoupling     = nDindex%MaxCoupling
        MinCoupling     = nDindex%MinCoupling
        MaxNorm         = nDindex%MaxNorm
        MinNorm         = nDindex%MinNorm
        Lmax            = nDindex%Lmax
        Lmin            = nDindex%Lmin
        With_L          = nDindex%With_L

        IF (allocated(nDindex%nDsize))    THEN
          CALL dealloc_NParray(nDindex%nDsize,"nDindex%nDsize","dealloc_nDindex")
        END IF

        IF (allocated(nDindex%nDweight))  THEN
          CALL dealloc_NParray(nDindex%nDweight,"nDindex%nDweight","dealloc_nDindex")
        END IF

        IF (allocated(nDindex%nDinit))  THEN
          CALL dealloc_NParray(nDindex%nDinit,"nDindex%nDinit","dealloc_nDindex")
        END IF

        IF (allocated(nDindex%nDend))     THEN
          CALL dealloc_NParray(nDindex%nDend,"nDindex%nDend","dealloc_nDindex")
        END IF

        IF (allocated(nDindex%skip_li)) THEN
          CALL dealloc_NParray(nDindex%skip_li,"nDindex%skip_li","dealloc_nDindex")
        END IF

        IF (allocated(nDindex%Tab_nDval))  THEN
          CALL dealloc_NParray(nDindex%Tab_nDval,"nDindex%Tab_nDval","dealloc_nDindex")
        END IF

        IF (allocated(nDindex%Tab_Norm))  THEN
          CALL dealloc_NParray(nDindex%Tab_Norm,"nDindex%Tab_Norm","dealloc_nDindex")
        END IF

        IF (allocated(nDindex%Tab_L))  THEN
          CALL dealloc_NParray(nDindex%Tab_L,"nDindex%Tab_L","dealloc_nDindex")
        END IF


        IF (allocated(nDindex%Tab_nDNorm)) THEN
          CALL dealloc_array(nDindex%Tab_nDNorm,"nDindex%Tab_nDNorm","dealloc_nDindex")
        END IF

        IF (allocated(nDindex%Tab_i_TO_L)) THEN
          CALL dealloc_array(nDindex%Tab_i_TO_L,"nDindex%Tab_i_TO_L","dealloc_nDindex")
        END IF

        IF (allocated(nDindex%nDNum_OF_Lmax)) THEN
          CALL dealloc_NParray(nDindex%nDNum_OF_Lmax,"nDindex%nDNum_OF_Lmax","dealloc_nDindex")
        END IF

        CALL dealloc_nDInd(nDindex%Tab_DInd)


        nDindex%alloc        = .FALSE. ! IF F, tables haven't been allocated
        nDindex%init         = .FALSE. ! IF F, tables haven't been initialized
        nDindex%Write_Tab    = .FALSE.

        nDindex%ndim         = 0   ! number of index: dimension of table nDind, nDdim


        nDindex%packed      = .FALSE.
        nDindex%packed_done = .FALSE.


        nDindex%Max_nDI = 0             ! largest value of the multidimensional index
        nDindex%type_OF_nDindex = -1    ! 0: such Sum(nDind(:)) < Norm
                                        ! 1: standard table, (init1....dim1)X(init2....dim2)....
        nDindex%MaxCoupling     = -1
        nDindex%MaxNorm = -ONE  ! Max Norm for the type_OF_nDindex = 0
        nDindex%MinNorm = ZERO  ! Min Norm for the type_OF_nDindex = 0



        nDindex%type_OF_nDindex = type_OF_nDindex
        nDindex%MaxNorm         = MaxNorm
        nDindex%MinNorm         = MinNorm
        nDindex%MaxCoupling     = MaxCoupling
        nDindex%MinCoupling     = MinCoupling
        nDindex%Lmax            = Lmax
        nDindex%Lmin            = Lmin
        nDindex%With_L          = With_L

      END SUBROUTINE dealloc_nDindex

      SUBROUTINE alloc_array_OF_nDindexdim0(tab,name_var,name_sub)
      IMPLICIT NONE

      TYPE (Type_nDindex), pointer, intent(inout) :: tab
      character (len=*),            intent(in) :: name_var,name_sub

      integer, parameter :: ndim=0
      logical :: memory_test

!----- for debuging --------------------------------------------------
      character (len=*), parameter :: name_sub_alloc = 'alloc_array_OF_nDindexdim0'
      integer :: err_mem,memory
      logical,parameter :: debug=.FALSE.
!      logical,parameter :: debug=.TRUE.
!----- for debuging --------------------------------------------------


       IF (associated(tab))                                             &
             CALL Write_error_NOT_null(name_sub_alloc,name_var,name_sub)

       memory = 1 ! true pointer
       allocate(tab,stat=err_mem)
       CALL error_memo_allo(err_mem,memory,name_var,name_sub,'Type_nDindex')

      END SUBROUTINE alloc_array_OF_nDindexdim0
      SUBROUTINE dealloc_array_OF_nDindexdim0(tab,name_var,name_sub)
      IMPLICIT NONE

      TYPE (Type_nDindex), pointer, intent(inout) :: tab
      character (len=*),            intent(in)    :: name_var,name_sub

!----- for debuging --------------------------------------------------
      character (len=*), parameter :: name_sub_alloc = 'dealloc_array_OF_nDindexdim0'
      integer :: err_mem,memory
      logical,parameter :: debug=.FALSE.
!      logical,parameter :: debug=.TRUE.
!----- for debuging --------------------------------------------------

       IF (.NOT. associated(tab))                                       &
             CALL Write_error_null(name_sub_alloc,name_var,name_sub)

       CALL dealloc_nDindex(tab)
       memory = 1
       deallocate(tab,stat=err_mem)
       CALL error_memo_allo(err_mem,-memory,name_var,name_sub,'Type_nDindex')
       nullify(tab)

      END SUBROUTINE dealloc_array_OF_nDindexdim0

      SUBROUTINE alloc_array_OF_nDindexdim1(tab,tab_ub,name_var,name_sub,tab_lb)
      IMPLICIT NONE

      TYPE (Type_nDindex), pointer, intent(inout) :: tab(:)
      integer, intent(in) :: tab_ub(:)
      integer, intent(in), optional :: tab_lb(:)

      character (len=*), intent(in) :: name_var,name_sub

      integer, parameter :: ndim=1
      logical :: memory_test

!----- for debuging --------------------------------------------------
      character (len=*), parameter :: name_sub_alloc = 'alloc_array_OF_nDindexdim1'
      integer :: err_mem,memory
      logical,parameter :: debug=.FALSE.
!      logical,parameter :: debug=.TRUE.
!----- for debuging --------------------------------------------------


       IF (associated(tab))                                             &
             CALL Write_error_NOT_null(name_sub_alloc,name_var,name_sub)

       CALL sub_test_tab_ub(tab_ub,ndim,name_sub_alloc,name_var,name_sub)

       IF (present(tab_lb)) THEN
         CALL sub_test_tab_lb(tab_lb,ndim,name_sub_alloc,name_var,name_sub)

         memory = product(tab_ub(:)-tab_lb(:)+1)
         allocate(tab(tab_lb(1):tab_ub(1)),stat=err_mem)
       ELSE
         memory = product(tab_ub(:))
         allocate(tab(tab_ub(1)),stat=err_mem)
       END IF
       CALL error_memo_allo(err_mem,memory,name_var,name_sub,'Type_nDindex')

      END SUBROUTINE alloc_array_OF_nDindexdim1
      SUBROUTINE dealloc_array_OF_nDindexdim1(tab,name_var,name_sub)
      IMPLICIT NONE

      TYPE (Type_nDindex), pointer, intent(inout) :: tab(:)

      integer :: i
      character (len=*), intent(in) :: name_var,name_sub

!----- for debuging --------------------------------------------------
      character (len=*), parameter :: name_sub_alloc = 'dealloc_array_OF_nDindexdim1'
      integer :: err_mem,memory
      logical,parameter :: debug=.FALSE.
!      logical,parameter :: debug=.TRUE.
!----- for debuging --------------------------------------------------

       IF (.NOT. associated(tab))                                       &
             CALL Write_error_null(name_sub_alloc,name_var,name_sub)

       DO i=lbound(tab,dim=1),ubound(tab,dim=1)
         CALL dealloc_nDindex(tab(i))
       END DO

       memory = size(tab)
       deallocate(tab,stat=err_mem)
       CALL error_memo_allo(err_mem,-memory,name_var,name_sub,'Type_nDindex')
       nullify(tab)

      END SUBROUTINE dealloc_array_OF_nDindexdim1

      SUBROUTINE alloc_NParray_OF_nDindexdim0(tab,name_var,name_sub)
      IMPLICIT NONE

      TYPE (Type_nDindex), allocatable, intent(inout) :: tab

      character (len=*), intent(in) :: name_var,name_sub

      integer, parameter :: ndim=0
      logical :: memory_test

!----- for debuging --------------------------------------------------
      character (len=*), parameter :: name_sub_alloc = 'alloc_NParray_OF_nDindexdim0'
      integer :: err_mem,memory
      logical,parameter :: debug=.FALSE.
!      logical,parameter :: debug=.TRUE.
!----- for debuging --------------------------------------------------


       IF (allocated(tab))                                             &
             CALL Write_error_NOT_null(name_sub_alloc,name_var,name_sub)

       memory = 1
       allocate(tab,stat=err_mem)
       CALL error_memo_allo(err_mem,memory,name_var,name_sub,'Type_nDindex')

      END SUBROUTINE alloc_NParray_OF_nDindexdim0
      SUBROUTINE dealloc_NParray_OF_nDindexdim0(tab,name_var,name_sub)
      IMPLICIT NONE

      TYPE (Type_nDindex), allocatable, intent(inout) :: tab
      character (len=*), intent(in) :: name_var,name_sub

!----- for debuging --------------------------------------------------
      character (len=*), parameter :: name_sub_alloc = 'dealloc_array_OF_nDindexdim0'
      integer :: err_mem,memory
      logical,parameter :: debug=.FALSE.
!      logical,parameter :: debug=.TRUE.
!----- for debuging --------------------------------------------------

       !IF (.NOT. allocated(tab)) RETURN
       IF (.NOT. allocated(tab))                                       &
             CALL Write_error_null(name_sub_alloc,name_var,name_sub)

       CALL dealloc_nDindex(tab)
       memory = 1
       deallocate(tab,stat=err_mem)
       CALL error_memo_allo(err_mem,-memory,name_var,name_sub,'Type_nDindex')

      END SUBROUTINE dealloc_NParray_OF_nDindexdim0

      SUBROUTINE alloc_NParray_OF_nDindexdim1(tab,tab_ub,name_var,name_sub,tab_lb)
      IMPLICIT NONE

      TYPE (Type_nDindex), allocatable, intent(inout) :: tab(:)
      integer, intent(in) :: tab_ub(:)
      integer, intent(in), optional :: tab_lb(:)

      character (len=*), intent(in) :: name_var,name_sub

      integer, parameter :: ndim=1
      logical :: memory_test

!----- for debuging --------------------------------------------------
      character (len=*), parameter :: name_sub_alloc = 'alloc_NParray_OF_nDindexdim1'
      integer :: err_mem,memory
      logical,parameter :: debug=.FALSE.
!      logical,parameter :: debug=.TRUE.
!----- for debuging --------------------------------------------------


       IF (allocated(tab))                                             &
             CALL Write_error_NOT_null(name_sub_alloc,name_var,name_sub)

       CALL sub_test_tab_ub(tab_ub,ndim,name_sub_alloc,name_var,name_sub)

       IF (present(tab_lb)) THEN
         CALL sub_test_tab_lb(tab_lb,ndim,name_sub_alloc,name_var,name_sub)

         memory = product(tab_ub(:)-tab_lb(:)+1)
         allocate(tab(tab_lb(1):tab_ub(1)),stat=err_mem)
       ELSE
         memory = product(tab_ub(:))
         allocate(tab(tab_ub(1)),stat=err_mem)
       END IF
       CALL error_memo_allo(err_mem,memory,name_var,name_sub,'Type_nDindex')

      END SUBROUTINE alloc_NParray_OF_nDindexdim1
      SUBROUTINE dealloc_NParray_OF_nDindexdim1(tab,name_var,name_sub)
      IMPLICIT NONE

      TYPE (Type_nDindex), allocatable, intent(inout) :: tab(:)

      integer :: i
      character (len=*), intent(in) :: name_var,name_sub

!----- for debuging --------------------------------------------------
      character (len=*), parameter :: name_sub_alloc = 'dealloc_NParray_OF_nDindexdim1'
      integer :: err_mem,memory
      logical,parameter :: debug=.FALSE.
!      logical,parameter :: debug=.TRUE.
!----- for debuging --------------------------------------------------

       !IF (.NOT. allocated(tab)) RETURN
       IF (.NOT. allocated(tab))                                       &
             CALL Write_error_null(name_sub_alloc,name_var,name_sub)

       DO i=lbound(tab,dim=1),ubound(tab,dim=1)
         CALL dealloc_nDindex(tab(i))
       END DO

       memory = size(tab)
       deallocate(tab,stat=err_mem)
       CALL error_memo_allo(err_mem,-memory,name_var,name_sub,'Type_nDindex')

      END SUBROUTINE dealloc_NParray_OF_nDindexdim1

!     ==================================================================
!     pack_nDindex of nDindex
!     ==================================================================
      !!@description: TODO
      !!@param: TODO
      SUBROUTINE pack_nDindex(nDindex,sort)

        TYPE (Type_nDindex), intent(inout) :: nDindex
        logical, optional :: sort

        integer :: nDI
        logical :: sort_loc


!----- for debuging --------------------------------------------------
      character (len=*), parameter :: name_sub='pack_nDindex'
      logical,parameter :: debug=.FALSE.
!      logical,parameter :: debug=.TRUE.
!-----------------------------------------------------------

      IF (nDindex%packed_done .OR. .NOT. nDindex%packed) RETURN

!-----------------------------------------------------------
      IF (debug) THEN
        write(out_unit,*) 'BEGINNING ',name_sub
      END IF
!-----------------------------------------------------------

        IF (.NOT. nDindex%init) THEN
          write(out_unit,*) ' ERROR in ',name_sub
          write(out_unit,*) ' nDindex has to be initialized!!'
          write(out_unit,*) ' Check the fortran source!!'
          STOP
        END IF

        sort_loc = .FALSE.
        IF (present(sort)) sort_loc = sort

        IF (.NOT. allocated(nDindex%Tab_nDval)) THEN
          CALL alloc_NParray(nDindex%Tab_nDval,                           &
                                      [nDindex%ndim,nDindex%Max_nDI], &
                          "nDindex%Tab_nDval",name_sub)
        END IF
        IF (.NOT. allocated(nDindex%Tab_Norm)) THEN
          CALL alloc_NParray(nDindex%Tab_Norm,[nDindex%Max_nDI],        &
                          "nDindex%Tab_Norm",name_sub)
        END IF

        IF (.NOT. allocated(nDindex%Tab_L)) THEN
          CALL alloc_NParray(nDindex%Tab_L,[nDindex%Max_nDI],         &
                            "nDindex%Tab_L",name_sub)
        END IF

        DO nDI=1,nDindex%Max_nDI
          CALL calc_nDindex(nDindex,nDI,nDindex%Tab_nDval(:,nDI))
          nDindex%Tab_Norm(nDI) = calc_Norm_OF_nDval(nDindex%Tab_nDval(:,nDI),nDindex)
          nDindex%Tab_L(nDI)    = calc_L_OF_nDval(nDindex%Tab_nDval(:,nDI),nDindex)
          !write(out_unit,*) 'nDI,nDval,Norm',nDI,nDindex%Tab_nDval(:,nDI),nDindex%Tab_Norm(nDI)

        END DO

        nDindex%packed_done = nDindex%packed

        IF (sort_loc) CALL sort_nDindex(nDindex)


!-----------------------------------------------------------
      IF (debug) THEN
        write(out_unit,*) 'END ',name_sub
      END IF
!-----------------------------------------------------------

      END SUBROUTINE pack_nDindex
      SUBROUTINE unpack_nDindex(nDindex)

        TYPE (Type_nDindex), intent(inout) :: nDindex

!----- for debuging --------------------------------------------------
      character (len=*), parameter :: name_sub='unpack_nDindex'
      logical,parameter :: debug=.FALSE.
!      logical,parameter :: debug=.TRUE.
!-----------------------------------------------------------

!-----------------------------------------------------------
      IF (debug) THEN
        write(out_unit,*) 'BEGINNING ',name_sub
      END IF
!-----------------------------------------------------------

        IF (allocated(nDindex%Tab_nDval)) THEN
          CALL dealloc_NParray(nDindex%Tab_nDval,"nDindex%Tab_nDval",name_sub)
        END IF

        IF (allocated(nDindex%Tab_Norm)) THEN
          CALL dealloc_NParray(nDindex%Tab_Norm,"nDindex%Tab_Norm",name_sub)
        END IF

        IF (allocated(nDindex%Tab_L)) THEN
          CALL dealloc_NParray(nDindex%Tab_L,"nDindex%Tab_L",name_sub)
        END IF

        nDindex%packed_done = .FALSE.
        nDindex%packed      = .FALSE.

!-----------------------------------------------------------
      IF (debug) THEN
        write(out_unit,*) 'END ',name_sub
      END IF
!-----------------------------------------------------------

      END SUBROUTINE unpack_nDindex
      SUBROUTINE sort_nDindex(nDindex)

        TYPE (Type_nDindex), intent(inout) :: nDindex

        integer               :: nDI,nDJ,Li,Lj
        real (kind=Rkind)     :: Normi,Normj
        integer, allocatable  :: nDval(:)


!----- for debuging --------------------------------------------------
      character (len=*), parameter :: name_sub='sort_nDindex'
      logical,parameter :: debug=.FALSE.
      !logical,parameter :: debug=.TRUE.
!-----------------------------------------------------------

!-----------------------------------------------------------
      IF (debug) THEN
        write(out_unit,*) 'BEGINNING ',name_sub
      END IF
!-----------------------------------------------------------

        IF (.NOT. nDindex%init) THEN
          write(out_unit,*) ' ERROR in ',name_sub
          write(out_unit,*) ' nDindex has to be initialized!!'
          write(out_unit,*) ' Check the fortran source!!'
          STOP
        END IF
        IF (.NOT. nDindex%packed_done) THEN
          write(out_unit,*) ' ERROR in ',name_sub
          write(out_unit,*) ' nDindex has to be packed!!'
          write(out_unit,*) ' Check the fortran source!!'
          STOP
        END IF

        CALL alloc_NParray(nDval,[nDindex%ndim],'nDval',name_sub)

        DO nDI=1,nDindex%Max_nDI
        DO nDJ=nDI+1,nDindex%Max_nDI
          Normi = nDindex%Tab_Norm(nDI)
          Normj = nDindex%Tab_Norm(nDJ)
          Li    = nDindex%Tab_L(nDI)
          Lj    = nDindex%Tab_L(nDJ)

          IF (Normi > Normj) THEN   ! permutation: d0b, d1b, d2b ...
            nDval(:)                 = nDindex%Tab_nDval(:,nDI)
            nDindex%Tab_nDval(:,nDI) = nDindex%Tab_nDval(:,nDJ)
            nDindex%Tab_nDval(:,nDJ) = nDval(:)

            nDindex%Tab_Norm(nDI)    = Normj
            nDindex%Tab_Norm(nDJ)    = Normi

            nDindex%Tab_L(nDI)       = Lj
            nDindex%Tab_L(nDJ)       = Li

          END IF

        END DO
        END DO

        CALL dealloc_NParray(nDval,'nDval',name_sub)

!-----------------------------------------------------------
      IF (debug) THEN
        write(out_unit,*) 'END ',name_sub
      END IF
!-----------------------------------------------------------

      END SUBROUTINE sort_nDindex

!     =================================================================
!     nDindex2 TO nDindex1 (not used)
!     =================================================================
      !!@description: TODO
      !!@param: TODO
      SUBROUTINE nDindex2TOnDindex1(nDindex1,nDindex2)
        CLASS (Type_nDindex), intent(inout) :: nDindex1
        TYPE (Type_nDindex),  intent(in)    :: nDindex2

        integer :: i
        integer :: err_mem,memory

        CALL dealloc_nDindex(nDindex1)
        IF (nDindex2%alloc) THEN
          CALL alloc_nDindex(nDindex1,nDindex2%ndim)
        END IF

        !CALL Write_nDindex(nDindex2)

        nDindex1%alloc = .TRUE.
        nDindex1%init  = .TRUE.

        nDindex1%ndim            = nDindex2%ndim
        nDindex1%Max_nDI         = nDindex2%Max_nDI
        nDindex1%type_OF_nDindex = nDindex2%type_OF_nDindex
        nDindex1%MaxNorm         = nDindex2%MaxNorm
        nDindex1%MinNorm         = nDindex2%MinNorm
        nDindex1%MaxCoupling     = nDindex2%MaxCoupling
        nDindex1%MinCoupling     = nDindex2%MinCoupling

        nDindex1%Lmax            = nDindex2%Lmax
        nDindex1%Lmin            = nDindex2%Lmin
        nDindex1%With_L          = nDindex2%With_L

        nDindex1%L1max           = nDindex2%L1max
        nDindex1%L2max           = nDindex2%L2max

        IF (allocated(nDindex2%nDNum_OF_Lmax)) THEN
          nDindex1%nDNum_OF_Lmax = nDindex2%nDNum_OF_Lmax
        END IF

        IF (allocated(nDindex2%nDinit)) THEN
          nDindex1%nDinit = nDindex2%nDinit
        END IF

        IF (allocated(nDindex2%nDend)) THEN
          nDindex1%nDend = nDindex2%nDend
        END IF

        IF (allocated(nDindex2%nDsize)) THEN
          nDindex1%nDsize = nDindex2%nDsize
        END IF


        IF (allocated(nDindex2%nDweight)) THEN
          nDindex1%nDweight = nDindex2%nDweight
        END IF

        nDindex1%packed         = nDindex2%packed
        nDindex1%packed_done    = nDindex2%packed_done

        IF (allocated(nDindex2%skip_li)) THEN
          CALL alloc_NParray(nDindex1%skip_li,shape(nDindex2%skip_li),          &
                             "nDindex1%skip_li","nDindex2TOnDindex1")
          nDindex1%skip_li = nDindex2%skip_li
        END IF

        IF (allocated(nDindex2%Tab_nDval)) THEN
          CALL alloc_NParray(nDindex1%Tab_nDval,                          &
                                   [nDindex1%ndim,nDindex1%Max_nDI],  &
                           "nDindex1%Tab_nDval","nDindex2TOnDindex1")
          nDindex1%Tab_nDval = nDindex2%Tab_nDval
        END IF

        IF (allocated(nDindex2%Tab_Norm)) THEN
          CALL alloc_NParray(nDindex1%Tab_Norm,[nDindex1%Max_nDI],   &
                          "nDindex1%Tab_Norm","nDindex2TOnDindex1")
          nDindex1%Tab_Norm = nDindex2%Tab_Norm
        END IF

        IF (allocated(nDindex2%Tab_L)) THEN
          CALL alloc_NParray(nDindex1%Tab_L,[nDindex1%Max_nDI],   &
                          "nDindex1%Tab_L","nDindex2TOnDindex1")
          nDindex1%Tab_L = nDindex2%Tab_L
        END IF


        IF (allocated(nDindex2%Tab_nDNorm)) THEN
          CALL alloc_array(nDindex1%Tab_nDNorm,[nDindex1%ndim],       &
                          "nDindex1%Tab_nDNorm","nDindex2TOnDindex1")
          DO i=1,nDindex1%ndim
            nDindex1%Tab_nDNorm(i) = nDindex2%Tab_nDNorm(i)
          END DO
        END IF

        IF (allocated(nDindex2%Tab_i_TO_L)) THEN
          CALL alloc_array(nDindex1%Tab_i_TO_L,[nDindex1%ndim],       &
                          "nDindex1%Tab_i_TO_L","nDindex2TOnDindex1")
          DO i=1,nDindex1%ndim
            nDindex1%Tab_i_TO_L(i) = nDindex2%Tab_i_TO_L(i)
          END DO
        END IF

        !nDindex1%Tab_DInd = nDindex2%Tab_DInd  ! it does not work with ifort (f2008??)
        CALL nDInd2TOnDInd1(nDindex1%Tab_DInd,nDindex2%Tab_DInd)
        !CALL Write_nDindex(nDindex1)

      END SUBROUTINE nDindex2TOnDindex1
      SUBROUTINE nDindex2TOnDindex1_InitOnly(nDindex1,nDindex2) !  (not used)
        TYPE (Type_nDindex), intent(inout) :: nDindex1
        TYPE (Type_nDindex), intent(in)    :: nDindex2

        integer :: i
        integer :: err_mem,memory

        CALL dealloc_nDindex(nDindex1)
        IF (nDindex2%alloc) THEN
          CALL alloc_nDindex(nDindex1,nDindex2%ndim)
        END IF

        !CALL Write_nDindex(nDindex2)

        nDindex1%alloc = .FALSE.
        nDindex1%init  = .TRUE.

        nDindex1%ndim            = nDindex2%ndim
        nDindex1%Max_nDI         = 0
        nDindex1%type_OF_nDindex = nDindex2%type_OF_nDindex
        nDindex1%MaxNorm         = nDindex2%MaxNorm
        nDindex1%MinNorm         = nDindex2%MinNorm
        nDindex1%MaxCoupling     = nDindex2%MaxCoupling
        nDindex1%MinCoupling     = nDindex2%MinCoupling

        nDindex1%Lmax            = nDindex2%Lmax
        nDindex1%Lmin            = nDindex2%Lmin
        nDindex1%With_L          = nDindex2%With_L

        nDindex1%L1max           = nDindex2%L1max
        nDindex1%L2max           = nDindex2%L2max

        IF (allocated(nDindex2%nDNum_OF_Lmax)) THEN
          nDindex1%nDNum_OF_Lmax = nDindex2%nDNum_OF_Lmax
        END IF


        IF (allocated(nDindex2%nDinit)) THEN
          nDindex1%nDinit = nDindex2%nDinit
        END IF

        IF (allocated(nDindex2%nDend)) THEN
          nDindex1%nDend = nDindex2%nDend
        END IF

        IF (allocated(nDindex2%nDsize)) THEN
          nDindex1%nDsize = nDindex2%nDsize
        END IF

        IF (allocated(nDindex2%nDweight)) THEN
          nDindex1%nDweight = nDindex2%nDweight
        END IF

        IF (allocated(nDindex2%skip_li)) THEN
          CALL alloc_NParray(nDindex1%skip_li,shape(nDindex2%skip_li),          &
                             "nDindex1%skip_li","nDindex2TOnDindex1")
          nDindex1%skip_li = nDindex2%skip_li
        END IF

        IF (allocated(nDindex2%Tab_nDNorm)) THEN
          CALL alloc_array(nDindex1%Tab_nDNorm,[nDindex1%ndim],       &
                          "nDindex1%Tab_nDNorm","nDindex2TOnDindex1")
          DO i=1,nDindex1%ndim
            nDindex1%Tab_nDNorm(i) = nDindex2%Tab_nDNorm(i)
          END DO
        END IF

        IF (allocated(nDindex2%Tab_i_TO_L)) THEN
          CALL alloc_array(nDindex1%Tab_i_TO_L,[nDindex1%ndim],       &
                          "nDindex1%Tab_i_TO_L","nDindex2TOnDindex1")
          DO i=1,nDindex1%ndim
            nDindex1%Tab_i_TO_L(i) = nDindex2%Tab_i_TO_L(i)
          END DO
        END IF

        nDindex1%packed         = nDindex2%packed
        nDindex1%packed_done    = .FALSE.


        !CALL Write_nDindex(nDindex1)

      END SUBROUTINE nDindex2TOnDindex1_InitOnly
!     =================================================================
!      Calculation of the multidimensional index as a function of a table
!      Example in 3D:
!      1 1 1 => 1
!      1 1 2 => 2
!      ...
!      1 1 n3 => n3
!      1 2  1 => n3  + 1
!     =================================================================
      !!@description: TODO
      !!@param: TODO
      SUBROUTINE calc_nDindex(nDindex,nDI,nDval,err_sub)
        TYPE (Type_nDindex), intent(in)    :: nDindex
        integer,             intent(in)    :: nDI
        integer,             intent(inout) :: nDval(:)
        integer,             intent(inout), optional     :: err_sub

        integer :: loc_nDI,i

        IF (present(err_sub)) err_sub = 0

        IF (.NOT. nDindex%init) THEN
          write(out_unit,*) ' ERROR in calc_nDindex'
          write(out_unit,*) ' nDindex is not initialized!'
          STOP
        END IF

        IF (nDI > nDindex%Max_nDI .OR. nDI <1) THEN
          write(out_unit,*) ' ERROR in calc_nDindex'
          CALL Write_nDindex(nDindex,"calc_nDindex: ")
          write(out_unit,*) ' nDI is larger than Max_nDI or nDI < 1',nDI,nDindex%Max_nDI
          write(out_unit,*) '  Check the source'

          IF (present(err_sub)) THEN
            err_sub = err_nDI
            RETURN
          ELSE
            STOP
          END IF

        END IF

        IF (nDindex%packed_done) THEN
            nDval(:) = nDindex%Tab_nDval(:,nDI)
        ELSE
          SELECT CASE (nDindex%type_OF_nDindex)
          CASE (1)
            CALL calc_nDval_p1(nDval,nDI,nDindex%nDsize,nDindex%ndim)
            nDval(:) = nDval(:) -1 + nDindex%nDinit(:) ! -1, because 1 is added in calc_nDindex_p1

            IF (any(nDval > nDindex%nDsize) .OR. any(nDval < nDindex%nDinit)) THEN
               IF (present(err_sub)) THEN
                 err_sub = err_nDval
               ELSE
                 write(out_unit,*) ' ERROR in calc_nDindex'
                 write(out_unit,*) '  Some nDval value are out of range!'
                 write(out_unit,*) '  nDval',nDval
                 write(out_unit,*) '  nDsize',nDindex%nDsize
                 STOP
               END IF
            END IF

          CASE (-1)
            CALL calc_nDval_m1(nDval,nDI,nDindex%nDsize,nDindex%ndim)
            nDval(:) = nDval(:) -1 + nDindex%nDinit(:) ! -1, because 1 is added in calc_nDindex_m1

            IF (any(nDval > nDindex%nDsize) .OR. any(nDval < nDindex%nDinit)) THEN
               IF (present(err_sub)) THEN
                 err_sub = err_nDval
               ELSE
                 write(out_unit,*) ' ERROR in calc_nDindex'
                 write(out_unit,*) '  Some nDval value are out of range!'
                 write(out_unit,*) '  nDval',nDval
                 write(out_unit,*) '  nDsize',nDindex%nDsize
                 STOP
               END IF
            END IF

          CASE (5)
            CALL calc_nDindex_type5p(nDindex,nDI,nDval)
          CASE (-5)
            CALL calc_nDindex_type5m(nDindex,nDI,nDval)

          CASE DEFAULT
            write(out_unit,*) ' ERROR in calc_nDindex'
            write(out_unit,*) ' Not yet type_OF_nDindex =',nDindex%type_OF_nDindex
            STOP

          END SELECT
        END IF


      END SUBROUTINE calc_nDindex

  SUBROUTINE calc_nDindex_type5p(nDindex,nDI,nDval)
    TYPE (Type_nDindex), intent(in)    :: nDindex
    integer,             intent(in)    :: nDI
    integer,             intent(inout) :: nDval(:)

    integer :: loc_nDI
    logical :: test

    CALL init_nDval_OF_nDindex(nDindex,nDval)
    DO loc_nDI=1,nDI
      CALL ADD_ONE_TO_nDindex_type5p(nDval,nDindex,test)
    END DO

  END SUBROUTINE calc_nDindex_type5p
  SUBROUTINE calc_nDindex_type5m(nDindex,nDI,nDval)
    TYPE (Type_nDindex), intent(in)    :: nDindex
    integer,             intent(in)    :: nDI
    integer,             intent(inout) :: nDval(:)

    integer :: loc_nDI
    logical :: test

    CALL init_nDval_OF_nDindex(nDindex,nDval)
    DO loc_nDI=1,nDI
      CALL ADD_ONE_TO_nDindex_type5m(nDval,nDindex,test)
    END DO

  END SUBROUTINE calc_nDindex_type5m
  SUBROUTINE calc_nDval_m1(nDval,nDI,nDsize,ndim)
  integer,             intent(in)    :: nDI,ndim
  integer,             intent(inout) :: nDval(:)
  integer,             intent(in)    :: nDsize(:)

  integer :: loc_nDI,i

  !write(out_unit,*) 'in calc_nDval_m1: nDI',nDI
  !write(out_unit,*) 'in calc_nDval_m1: nDsize',nDsize
  !write(out_unit,*) 'in calc_nDval_m1: ndim',ndim

  nDval(:) = 0
  loc_nDI = nDI-1
  DO i=1,ndim
    nDval(i) = mod(loc_nDI,nDsize(i))+1
    loc_nDI = int(loc_nDI/nDsize(i))
  END DO
  !write(out_unit,*) 'in calc_nDval_m1: nDval',nDval


  END SUBROUTINE calc_nDval_m1
  SUBROUTINE calc_nDval_p1(nDval,nDI,nDsize,ndim)
  integer,             intent(in)    :: nDI,ndim
  integer,             intent(inout) :: nDval(:)
  integer,             intent(in)    :: nDsize(:)

  integer :: loc_nDI,i

  nDval(:) = 0
  loc_nDI = nDI-1
  DO i=ndim,1,-1
    nDval(i) = mod(loc_nDI,nDsize(i))+1
    loc_nDI  = int(loc_nDI/nDsize(i))
  END DO

  END SUBROUTINE calc_nDval_p1

  SUBROUTINE init_nDval_OF_nDindex(nDindex,nDval,err_sub)
    TYPE (Type_nDindex), intent(in)    :: nDindex
    integer,             intent(inout) :: nDval(:)
    integer,             intent(inout), optional     :: err_sub

    integer :: nDI,i
    logical :: test

    IF (present(err_sub)) err_sub = 0

    IF (.NOT. nDindex%init) THEN
      write(out_unit,*) ' ERROR in init_nDval_OF_nDindex'
      write(out_unit,*) ' nDindex is not initialized!'
      STOP
    END IF
!write(out_unit,*) 'shape nDval',shape(nDval) ; flush(out_unit)
!write(out_unit,*) 'shape nDinit',shape(nDindex%nDinit) ; flush(out_unit)

    nDval(:) = nDindex%nDinit(:)

    SELECT CASE (nDindex%type_OF_nDindex)
    CASE (1)
      nDval(nDindex%ndim) = nDval(nDindex%ndim) -1
    CASE (-1)
      nDval(1) = nDval(1) -1

    CASE (2)
      nDval(nDindex%ndim) = nDval(nDindex%ndim) -1

    CASE (5)
      nDval(nDindex%ndim) = nDval(nDindex%ndim) -1
    CASE (-5)
      nDval(1) = nDval(1) -1

    CASE DEFAULT
      write(out_unit,*) ' ERROR in calc_nDindex'
      write(out_unit,*) ' Not yet type_OF_nDindex =',nDindex%type_OF_nDindex
      STOP

    END SELECT

  END SUBROUTINE init_nDval_OF_nDindex

  SUBROUTINE ADD_ONE_TO_nDindex(nDindex,nDval,iG,err_sub)
    TYPE (Type_nDindex), intent(in)                  :: nDindex
    integer,             intent(inout)               :: nDval(:)
    integer,             intent(in),    optional     :: iG
    integer,             intent(inout), optional     :: err_sub

    integer :: i,err_sub_loc
    logical :: test

    integer, save :: nDI = 1


!----------------------------------------------------------
      character (len=*), parameter :: name_sub='ADD_ONE_TO_nDindex'
      logical,parameter :: debug=.FALSE.
      !logical,parameter :: debug=.TRUE.
!-----------------------------------------------------------
    IF (debug) THEN
      write(out_unit,*) ' BEGINNING ',name_sub
      write(out_unit,*) ' in nDval',nDval
      flush(out_unit)
    END IF

    IF (present(err_sub)) err_sub = 0

    IF (.NOT. nDindex%init) THEN
      write(out_unit,*) ' ERROR in ',name_sub
      write(out_unit,*) ' nDindex is not initialized!'
      STOP
    END IF

    IF (nDindex%packed_done) THEN
      IF (present(iG)) THEN
        nDval(:) = nDindex%Tab_nDval(:,iG)
      ELSE
        IF (any(nDval < nDindex%nDinit(:))) THEN
          nDval(:) = nDindex%Tab_nDval(:,1)
          nDI = 1
        ELSE IF (any(nDval > nDindex%nDend(:))) THEN
          nDval(:) = nDindex%Tab_nDval(:,nDindex%Max_nDI)
          nDval(1) = nDval(1) + 1
          nDI = nDindex%Max_nDI + 1
        ELSE
          CALL calc_nDI(nDI,nDval,nDindex,err_sub_loc)
          IF (err_sub_loc == 0 .AND. nDI < nDindex%Max_nDI) THEN
            nDval(:) = nDindex%Tab_nDval(:,nDI+1)
          ELSE
            nDval(:) = nDindex%Tab_nDval(:,nDindex%Max_nDI)
            nDval(1) = nDval(1) + 1
          END IF
        END IF
      END IF
    ELSE
      SELECT CASE (nDindex%type_OF_nDindex)
      CASE (1)
        CALL ADD_ONE_TO_nDval_p1(nDval,nDindex%nDsize)
        nDval(:) = nDval(:) -1 + nDindex%nDinit(:) ! -1, because 1 is added in calc_nDindex_p1

      CASE (-1)
        CALL ADD_ONE_TO_nDval_m1(nDval,nDindex%nDsize)
        nDval(:) = nDval(:) -1 + nDindex%nDinit(:) ! -1, because 1 is added in calc_nDindex_m1

      CASE (2)
        CALL ADD_ONE_TO_nDindex_type2(nDval,nDindex)

      CASE (5)
        CALL ADD_ONE_TO_nDindex_type5p(nDval,nDindex,test)
      CASE (-5)
        CALL ADD_ONE_TO_nDindex_type5m(nDval,nDindex,test)
      CASE DEFAULT
        write(out_unit,*) ' ERROR in ',name_sub
        write(out_unit,*) ' Not yet type_OF_nDindex =',nDindex%type_OF_nDindex
        STOP

      END SELECT

      IF (any(nDval > nDindex%nDend) .OR. any(nDval < nDindex%nDinit)) THEN
         IF (present(err_sub)) THEN
           err_sub = err_nDval
         ELSE
           write(out_unit,*) ' ERROR in ',name_sub
           write(out_unit,*) '  Some nDval value are out of range!'
           write(out_unit,*) '  nDval',nDval
           write(out_unit,*) '  nDsize',nDindex%nDsize
           STOP
         END IF
      END IF

    END IF

    IF (debug) THEN
      write(out_unit,*) ' out nDval',nDval
      write(out_unit,*) ' END ',name_sub
      flush(out_unit)
    END IF


  END SUBROUTINE ADD_ONE_TO_nDindex

  SUBROUTINE ADD_ONE_TO_nDindex_type2(nDval,nDindex)
  integer,             intent(inout) :: nDval(:)
  TYPE (Type_nDindex), intent(in)    :: nDindex

  integer :: i,nb_Coupling
  logical :: test
  real(kind=Rkind) :: Norm

  nDval(nDindex%ndim) = nDval(nDindex%ndim) + 1
  DO i=nDindex%ndim,1,-1
    test = .TRUE.
    IF (nDval(i) <= nDindex%nDend(i)) THEN
      Norm = calc_Norm_OF_nDval(nDval,nDindex)
      nb_Coupling = count((nDval-nDindex%nDinit) > 0)
      test = Norm > nDindex%MaxNorm .OR. Norm < nDindex%MinNorm .OR.    &
             nb_Coupling > nDindex%MaxCoupling .OR.                     &
             nb_Coupling < nDindex%MinCoupling
    END IF

    IF (test) THEN
      nDval(i) = nDindex%nDinit(i)
      IF (i>1) nDval(i-1) = nDval(i-1) + 1
    ELSE
      EXIT
    END IF
  END DO


  END SUBROUTINE ADD_ONE_TO_nDindex_type2

  SUBROUTINE ADD_ONE_TO_nDindex_type5p(nDval,nDindex,In_the_list)
  integer,             intent(inout) :: nDval(:)
  TYPE (Type_nDindex), intent(in)    :: nDindex
  logical,             intent(inout) :: In_the_list

  integer :: i,nb_Coupling,L,L1,L2

  logical :: InRange

!-----------------------------------------------------------
      character (len=*), parameter :: name_sub='ADD_ONE_TO_nDindex_type5p'
      logical,parameter :: debug=.FALSE.
      !logical,parameter :: debug=.TRUE.
!-----------------------------------------------------------
      IF (debug) THEN
        write(out_unit,*) 'BEGINNING ',name_sub
        write(out_unit,*) '  nDval (in) ',nDval
        write(out_unit,*) '  nDend  ',nDindex%nDend
        write(out_unit,*) '  nDinit ',nDindex%nDinit
        write(out_unit,*) '  nDindex%Lmax',nDindex%Lmax

      END IF
!-----------------------------------------------------------

  DO
    nDval(nDindex%ndim) = nDval(nDindex%ndim) + 1

    CALL Analysis_nDval_nDindex_type5(nDval,nDindex,L,In_the_list,InRange)
    IF (debug) THEN
      write(out_unit,*) 'nDval (temp),L',nDval,':',L
      write(out_unit,*) 'nDval (temp)',nDval,':',In_the_list,InRange
    END IF


    IF ( nDval(nDindex%ndim) > nDindex%nDend(nDindex%ndim) .OR. L > nDindex%Lmax) THEN

      DO i=nDindex%ndim,2,-1
        nDval(i)   = nDindex%nDinit(i)
        nDval(i-1) = nDval(i-1) + 1

        CALL Analysis_nDval_nDindex_type5(nDval,nDindex,L,In_the_list,InRange)

        IF (debug) THEN
          write(out_unit,*) 'nDval2 (temp),L',nDval,':',L
          write(out_unit,*) 'nDval2 (temp)',nDval,':',In_the_list,InRange
        END IF

        IF (In_the_list) EXIT

      END DO
    END IF

    IF (nDval(1) > nDindex%nDend(1) .OR. L > nDindex%Lmax) EXIT

    nb_Coupling = count((nDval-nDindex%nDinit) > 0)
    IF ( nb_Coupling < nDindex%MinCoupling) CYCLE

    IF (In_the_list) EXIT

  END DO

  IF (debug) THEN
    write(out_unit,*) '  nDval (out), In_the_list',nDval,In_the_list
    write(out_unit,*) 'END ',name_sub
  END IF

END SUBROUTINE ADD_ONE_TO_nDindex_type5p
  SUBROUTINE ADD_ONE_TO_nDindex_type5m(nDval,nDindex,In_the_list)
  integer,             intent(inout) :: nDval(:)
  TYPE (Type_nDindex), intent(in)    :: nDindex
  logical,             intent(inout) :: In_the_list

  integer :: i,nb_Coupling,L,L1,L2
  logical :: InRange

!-----------------------------------------------------------
      character (len=*), parameter :: name_sub='ADD_ONE_TO_nDindex_type5m'
      logical,parameter :: debug=.FALSE.
      !logical,parameter :: debug=.TRUE.
!-----------------------------------------------------------
      IF (debug) THEN
        write(out_unit,*) 'BEGINNING ',name_sub
        write(out_unit,*) '  nDval (in) ',nDval
      END IF
!-----------------------------------------------------------

  DO
    nDval(1) = nDval(1) + 1
    CALL Analysis_nDval_nDindex_type5(nDval,nDindex,L,In_the_list,InRange)
    IF (debug) write(out_unit,*) 'nDval (temp)',nDval,In_the_list,InRange


    IF ( nDval(1) > nDindex%nDend(1) .OR. L > nDindex%Lmax) THEN

      DO i=1,nDindex%ndim-1
        nDval(i)   = nDindex%nDinit(i)
        nDval(i+1) = nDval(i+1) + 1

        CALL Analysis_nDval_nDindex_type5(nDval,nDindex,L,In_the_list,InRange)
        IF (debug) write(out_unit,*) 'nDval2 (temp)',nDval,In_the_list,InRange

        IF (In_the_list .OR. L < nDindex%Lmin) EXIT

      END DO
    END IF


    IF (nDval(nDindex%ndim) > nDindex%nDend(nDindex%ndim) .OR. L > nDindex%Lmax) EXIT

    nb_Coupling = count((nDval-nDindex%nDinit) > 0)
    IF ( nb_Coupling < nDindex%MinCoupling) CYCLE

    IF (In_the_list) EXIT

  END DO

  IF (debug) THEN
    write(out_unit,*) '  nDval (out), In_the_list',nDval,In_the_list
    write(out_unit,*) 'END ',name_sub
  END IF

  END SUBROUTINE ADD_ONE_TO_nDindex_type5m
  SUBROUTINE calc_LL1L2_OF_nDindex_type5(L,L1,L2,nDval,nDindex)

  TYPE (Type_nDindex), intent(in)        :: nDindex
  integer,             intent(inout)     :: L,L1,L2
  integer,             intent(in)        :: nDval(:)

  integer         :: i,vali

    IF (allocated(nDindex%Tab_i_TO_l)) THEN
      L1 = 0
      L2 = 0
      L  = 0

      DO i=1,nDindex%ndim
        vali = nDval(i)
        IF (nDindex%nDinit(i) == 0) vali = vali + 1

        IF (vali > ubound(nDindex%Tab_i_TO_l(i)%vec,dim=1)) THEN
          IF (nDindex%nDNum_OF_Lmax(i) == 1) THEN
            L1 = L1 + nDindex%L1max+1
          ELSE IF (nDindex%nDNum_OF_Lmax(i) == 2) THEN
            L2 = L2 + nDindex%L2max+1
          ELSE
            L  = L + nDindex%Lmax+1
          END IF
        ELSE
          IF (nDindex%nDNum_OF_Lmax(i) == 1) THEN
            L1 = L1 + nDindex%Tab_i_TO_l(i)%vec(vali)
          ELSE IF (nDindex%nDNum_OF_Lmax(i) == 2) THEN
            L2 = L2 + nDindex%Tab_i_TO_l(i)%vec(vali)
          ELSE
            L  = L + nDindex%Tab_i_TO_l(i)%vec(vali)
          END IF
        END IF
      END DO
      L = L + L1 + L2
    ELSE
      L1 = sum(nDval-nDindex%nDinit,mask=(nDindex%nDNum_OF_Lmax(:) == 1) )
      L2 = sum(nDval-nDindex%nDinit,mask=(nDindex%nDNum_OF_Lmax(:) == 2) )
      L  = L1 + L2 + sum(nDval-nDindex%nDinit,mask=(nDindex%nDNum_OF_Lmax(:) == 0) )
      IF (L /= sum(nDval-nDindex%nDinit)) STOP 'pb with L, L1, L2'
    END IF

  END SUBROUTINE calc_LL1L2_OF_nDindex_type5

  SUBROUTINE Analysis_nDval_nDindex_type5(nDval,nDindex,L,InList,InRange)

  integer,             intent(in)        :: nDval(:)
  TYPE (Type_nDindex), intent(in)        :: nDindex
  integer,             intent(inout)     :: L
  logical,             intent(inout)     :: InList,InRange


  integer             :: i,L1,L2,nb_Coupling
  logical, parameter  :: debug = .FALSE.



  CALL calc_LL1L2_OF_nDindex_type5(L,L1,L2,nDval,nDindex)
  nb_Coupling = count((nDval-nDindex%nDinit) > 0)

  InRange = L1 <= nDindex%L1max          .AND. L2 <= nDindex%L2max .AND.        &
            L  <= nDindex%Lmax           .AND. L  >= nDindex%Lmin  .AND.        &
            all(nDval <= nDindex%nDend)

  InList = InRange  .AND. nb_Coupling <= nDindex%MaxCoupling
  !InList = InRange  .AND. nb_Coupling <= nDindex%MaxCoupling .AND.              &
  !         nb_Coupling >= nDindex%MinCoupling

  ! it has to be this way because nDval(:) values can be larger than nDindex%nDend
  ! ... and therefore to the 1st dim range of skip_li(:,:)
  !write(6,*) 'nDval-nDindex%nDinit',nDval-nDindex%nDinit
  !write(6,*) 'ubound(nDindex%skip_li,dim=1)',ubound(nDindex%skip_li,dim=1)
  !write(6,*) 'lbound(nDindex%skip_li,dim=1)',lbound(nDindex%skip_li,dim=1)
  !flush(6)

  !IF (InList) InList = .NOT. any(nDindex%skip_li(nDval-nDindex%nDinit,:))

  IF (debug) THEN
    write(out_unit,*) '-----------------------------------------------------'
    write(out_unit,*) 'nDend      ',nDindex%nDend
    write(out_unit,*) 'nDinit     ',nDindex%nDinit
    write(out_unit,*) 'Lmax       ',nDindex%Lmax
    write(out_unit,*) 'nDval      ',nDval
    DO i=1,nDindex%ndim
      write(out_unit,*) i,'skip_li(:,i))',nDindex%skip_li(:,i)
    END DO
    write(out_unit,*) 'L1,L2,L    ',L1,L2,L
    write(out_unit,*) 'nb_Coupling',nb_Coupling
    flush(out_unit)
  END IF

END SUBROUTINE Analysis_nDval_nDindex_type5


  SUBROUTINE ADD_ONE_TO_nDval_m1(nDval,nDsize)
  integer,             intent(inout) :: nDval(:)
  integer,             intent(in)    :: nDsize(:)

  integer :: i

  nDval(1) = nDval(1) + 1
  DO i=1,size(nDval)-1
    IF ( nDval(i) > nDsize(i) ) THEN
      nDval(i)   = 1
      nDval(i+1) = nDval(i+1) + 1
    ELSE
      EXIT
    END IF
  END DO

  END SUBROUTINE ADD_ONE_TO_nDval_m1
  SUBROUTINE ADD_ONE_TO_nDval_p1(nDval,nDsize)
  integer,             intent(inout) :: nDval(:)
  integer,             intent(in)    :: nDsize(:)

  integer :: i

  nDval(size(nDval)) = nDval(size(nDval)) + 1
  DO i=size(nDval),2,-1
    IF ( nDval(i) > nDsize(i) ) THEN
      nDval(i)   = 1
      nDval(i-1) = nDval(i-1) + 1
    ELSE
      EXIT
    END IF
  END DO

  END SUBROUTINE ADD_ONE_TO_nDval_p1

  SUBROUTINE calc_nDI(nDI,nDval,nDindex,err_sub)
    TYPE (Type_nDindex), intent(in)               :: nDindex
    integer,             intent(in)               :: nDval(:)
    integer,             intent(inout)            :: nDI
    integer,             intent(inout), optional  :: err_sub

    integer :: i,ib,ibm,ibp,nDval_tmp(nDindex%ndim)
    logical :: not_out_of_range,found

!------------------------------------------------------
  character (len=*), parameter :: name_sub='calc_nDI'
  logical,parameter :: debug=.FALSE.
  !logical,parameter :: debug=.TRUE.
!-------------------------------------------------------
  IF (debug) THEN
    write(out_unit,*) 'BEGINNING ',name_sub
    write(out_unit,*) '  nDval (in) ',nDval
    !CALL write_nDindex(nDindex)
    flush(out_unit)
  END IF
!-------------------------------------------------------

  IF (present(err_sub)) err_sub = 0


  IF (.NOT. nDindex%init) THEN
    write(out_unit,*) ' ERROR in calc_nDI'
    write(out_unit,*) ' nDindex is not initialized!'
    STOP
  END IF

  IF (nDindex%packed) THEN

    IF (nDI < 1 .OR. nDI > nDindex%Max_nDI) nDI = 1

    ib = nDI

    ! first at nDI
    found = ( all(nDval == nDindex%Tab_nDval(:,ib)) )
    IF (debug .AND. found) write(out_unit,*) 'found at nDI',ib
    IF (debug .AND. .NOT. found) write(out_unit,*) 'not found at nDI',ib

    flush(out_unit)

    IF (.NOT. found) THEN

      ! then from nDI+1 to Max_nDI
      ibp = NDI  ! this index increases
      ibm = NDI  ! this index decreases

      DO
        IF (ibp < nDindex%Max_nDI) THEN
          ibp = ibp + 1
          found = ( all(nDval == nDindex%Tab_nDval(:,ibp)) )

          IF (found) THEN
            ib = ibp
            IF (debug) write(out_unit,*) 'found in [nDI+1 ... Max_nDI], it',ib-nDI
            EXIT
          END IF
        END IF

        IF (ibm > 1) THEN
          ibm = ibm - 1
          found = ( all(nDval == nDindex%Tab_nDval(:,ibm)) )
          IF (found) THEN
            ib = ibm
            IF (debug) write(out_unit,*) 'found in [1 ... nDI-1], it',nDI-ib
            EXIT
          END IF
        END IF

        IF (ibm == 1 .AND. ibp == nDindex%Max_nDI) EXIT

      END DO
    END IF

    IF (found) THEN
      nDI = ib
    ELSE
      IF (present(err_sub)) THEN
        nDI = nDindex%Max_nDI + 1
        err_sub = err_nDI
      ELSE
        write(out_unit,*) ' ERROR in calc_nDI'
        write(out_unit,*) ' nDI cannot be found !!'
        write(out_unit,*) ' CHECK the fortran source!'
        STOP
      END IF
    END IF

  ELSE
    SELECT CASE (nDindex%type_OF_nDindex)
    CASE (1)
      ib = nDval(1)
      not_out_of_range = (nDval(1) <= nDindex%nDend(1))
      !write(out_unit,*) 'ib',ib,not_out_of_range
      DO i=2,nDindex%ndim
        ib = (ib-1) * nDindex%nDend(i) + nDval(i)
        not_out_of_range = not_out_of_range .AND. (nDval(i) <= nDindex%nDend(i))
        !write(out_unit,*) 'ib',ib,not_out_of_range
      END DO

      IF (.NOT. not_out_of_range) THEN
        nDI = nDindex%Max_nDI + 1
      ELSE
        nDI = ib
      END IF

    CASE (-1)
      ib = nDval(nDindex%ndim)
      not_out_of_range = (nDval(nDindex%ndim) <= nDindex%nDend(nDindex%ndim))
      !write(out_unit,*) 'ib',ib,not_out_of_range
      DO i=nDindex%ndim-1,1,-1
        ib = (ib-1) * nDindex%nDend(i) + nDval(i)
        not_out_of_range = not_out_of_range .AND. (nDval(i) <= nDindex%nDend(i))
        !write(out_unit,*) 'ib',ib,not_out_of_range
      END DO

      IF (.NOT. not_out_of_range) THEN
        nDI = nDindex%Max_nDI + 1
      ELSE
        nDI = ib
      END IF

    CASE (2,-3,3,-4,4,-5,5)

      CALL init_nDval_OF_nDindex(nDindex,nDval_tmp)
      DO ib=1,nDindex%Max_nDI
        CALL ADD_ONE_TO_nDindex(nDindex,nDval_tmp)
        IF (all(nDval_tmp == nDval)) EXIT
      END DO
      nDI = ib


    CASE DEFAULT
      write(out_unit,*) ' ERROR in calc_nDI'
      write(out_unit,*) ' You cannot use type_OF_nDindex/=1 with packed=F'
      write(out_unit,*) '           OR'
      write(out_unit,*) ' Not yet this type_OF_nDindex :',nDindex%type_OF_nDindex
      write(out_unit,*) ' CHECK the fortran source!'
      STOP
    END SELECT

  END IF

!-------------------------------------------------------
  IF (debug) THEN
    write(out_unit,*) '  nDI ',nDI
    write(out_unit,*) 'END ',name_sub
    flush(out_unit)
  END IF
!-------------------------------------------------------

  END SUBROUTINE calc_nDI

  SUBROUTINE calc_nDI_v1(nDI,nDval,nDindex,err_sub)
    TYPE (Type_nDindex)    :: nDindex
    integer, intent(in)    :: nDval(:)
    integer, intent(inout) :: nDI
    integer,             intent(inout), optional     :: err_sub

    integer :: i,ib,nDval_tmp(nDindex%ndim)
    logical :: not_out_of_range,found

!------------------------------------------------------
  character (len=*), parameter :: name_sub='calc_nDI_v1'
  logical,parameter :: debug=.FALSE.
  !logical,parameter :: debug=.TRUE.
!-------------------------------------------------------
  IF (debug) THEN
    write(out_unit,*) 'BEGINNING ',name_sub
    write(out_unit,*) '  nDval (in) ',nDval
    !CALL write_nDindex(nDindex)
    flush(out_unit)
  END IF
!-------------------------------------------------------

  IF (present(err_sub)) err_sub = 0


  IF (.NOT. nDindex%init) THEN
    write(out_unit,*) ' ERROR in ',name_sub
    write(out_unit,*) ' nDindex is not initialized!'
    STOP
  END IF

  IF (nDindex%packed) THEN

    IF (nDI < 1 .OR. nDI > nDindex%Max_nDI) nDI = 1

    ib = nDI

    ! first at nDI
    found = ( all(nDval == nDindex%Tab_nDval(:,ib)) )
    IF (debug .AND. found) write(out_unit,*) 'found at nDI',ib
    IF (debug .AND. .NOT. found) write(out_unit,*) 'not found at nDI',ib

    flush(out_unit)

    ! then from nDI+1 to Max_nDI
    IF (.NOT. found) THEN
      DO ib=nDI+1,nDindex%Max_nDI
       found = ( all(nDval == nDindex%Tab_nDval(:,ib)) )
       IF (found) EXIT
      END DO
      IF (debug .AND. found) write(out_unit,*) 'found in [nDI+1 ... Max_nDI], it',ib-nDI
      IF (debug .AND. .NOT. found) write(out_unit,*) 'not found in [nDI+1 ... Max_nDI], it',ib-nDI

      !flush(out_unit)

    END IF

    ! then from 1 to nDI
    IF (.NOT. found) THEN
      DO ib=1,nDI-1
       found = ( all(nDval == nDindex%Tab_nDval(:,ib)) )
       IF (found) EXIT
      END DO
      IF (debug .AND. found) write(out_unit,*) 'found in [1 ... nDI-1], it',ib
      !flush(out_unit)

    END IF

    IF (found) THEN
      nDI = ib
    ELSE
      IF (present(err_sub)) THEN
        nDI = nDindex%Max_nDI + 1
        err_sub = err_nDI
      ELSE
        write(out_unit,*) ' ERROR in ',name_sub
        write(out_unit,*) ' nDI cannot be found !!'
        write(out_unit,*) ' CHECK the fortran source!'
        STOP
      END IF
    END IF

  ELSE
    SELECT CASE (nDindex%type_OF_nDindex)
    CASE (1)
      ib = nDval(1)
      not_out_of_range = (nDval(1) <= nDindex%nDend(1))
      !write(out_unit,*) 'ib',ib,not_out_of_range
      DO i=2,nDindex%ndim
        ib = (ib-1) * nDindex%nDend(i) + nDval(i)
        not_out_of_range = not_out_of_range .AND. (nDval(i) <= nDindex%nDend(i))
        !write(out_unit,*) 'ib',ib,not_out_of_range
      END DO

      IF (.NOT. not_out_of_range) THEN
        nDI = nDindex%Max_nDI + 1
      ELSE
        nDI = ib
      END IF

    CASE (-1)
      ib = nDval(nDindex%ndim)
      not_out_of_range = (nDval(nDindex%ndim) <= nDindex%nDend(nDindex%ndim))
      !write(out_unit,*) 'ib',ib,not_out_of_range
      DO i=nDindex%ndim-1,1,-1
        ib = (ib-1) * nDindex%nDend(i) + nDval(i)
        not_out_of_range = not_out_of_range .AND. (nDval(i) <= nDindex%nDend(i))
        !write(out_unit,*) 'ib',ib,not_out_of_range
      END DO

      IF (.NOT. not_out_of_range) THEN
        nDI = nDindex%Max_nDI + 1
      ELSE
        nDI = ib
      END IF

    CASE (2,-3,3,-4,4,-5,5)

      CALL init_nDval_OF_nDindex(nDindex,nDval_tmp)
      DO ib=1,nDindex%Max_nDI
        CALL ADD_ONE_TO_nDindex(nDindex,nDval_tmp)
        IF (all(nDval_tmp == nDval)) EXIT
      END DO
      nDI = ib


    CASE DEFAULT
      write(out_unit,*) ' ERROR in ',name_sub
      write(out_unit,*) ' You cannot use type_OF_nDindex/=1 with packed=F'
      write(out_unit,*) '           OR'
      write(out_unit,*) ' Not yet this type_OF_nDindex :',nDindex%type_OF_nDindex
      write(out_unit,*) ' CHECK the fortran source!'
      STOP
    END SELECT

  END IF

!-------------------------------------------------------
  IF (debug) THEN
    write(out_unit,*) '  nDI ',nDI
    write(out_unit,*) 'END ',name_sub
    flush(out_unit)
  END IF
!-------------------------------------------------------

  END SUBROUTINE calc_nDI_v1

  SUBROUTINE calc_nDI_old(nDI,nDval,nDindex,err_sub)
    TYPE (Type_nDindex)    :: nDindex
    integer, intent(in)    :: nDval(:)
    integer, intent(inout) :: nDI
    integer,             intent(inout), optional     :: err_sub

    integer :: i,ib,nDval_tmp(nDindex%ndim)
    logical :: not_out_of_range,found

!------------------------------------------------------
  character (len=*), parameter :: name_sub='calc_nDI_old'
  logical,parameter :: debug=.FALSE.
  !logical,parameter :: debug=.TRUE.
!-------------------------------------------------------
  IF (debug) THEN
    write(out_unit,*) 'BEGINNING ',name_sub
    write(out_unit,*) '  nDval (in) ',nDval
    !CALL write_nDindex(nDindex)
    flush(out_unit)
  END IF
!-------------------------------------------------------

  IF (present(err_sub)) err_sub = 0


  IF (.NOT. nDindex%init) THEN
    write(out_unit,*) ' ERROR in calc_nDI'
    write(out_unit,*) ' nDindex is not initialized!'
    STOP
  END IF

  IF (nDindex%packed) THEN

    IF (nDI < 1 .OR. nDI > nDindex%Max_nDI) nDI = 1

    ib = nDI

    ! first at nDI
    found = ( all(nDval == nDindex%Tab_nDval(:,ib)) )
    IF (debug .AND. found) write(out_unit,*) 'found at nDI',ib
    !flush(out_unit)

    ! then from nDI+1 to Max_nDI
    IF (.NOT. found) THEN
      DO ib=nDI+1,nDindex%Max_nDI
       found = ( all(nDval == nDindex%Tab_nDval(:,ib)) )
       IF (found) EXIT
      END DO
      IF (debug .AND. found) write(out_unit,*) 'found in [nDI+1 ... Max_nDI], it',ib-nDI
      !flush(out_unit)

    END IF

    ! then from 1 to nDI
    IF (.NOT. found) THEN
      DO ib=1,nDI-1
       found = ( all(nDval == nDindex%Tab_nDval(:,ib)) )
       IF (found) EXIT
      END DO
      IF (debug .AND. found) write(out_unit,*) 'found in [1 ... nDI-1], it',ib
      !flush(out_unit)

    END IF

    IF (found) THEN
      nDI = ib
    ELSE
      IF (present(err_sub)) THEN
        nDI = nDindex%Max_nDI + 1
        err_sub = err_nDI
      ELSE
        write(out_unit,*) ' ERROR in calc_nDI'
        write(out_unit,*) ' nDI cannot be found !!'
        write(out_unit,*) ' CHECK the fortran source!'
        STOP
      END IF
    END IF

  ELSE
    SELECT CASE (nDindex%type_OF_nDindex)
    CASE (1)
      ib = nDval(1)
      not_out_of_range = (nDval(1) <= nDindex%nDend(1))
      !write(out_unit,*) 'ib',ib,not_out_of_range
      DO i=2,nDindex%ndim
        ib = (ib-1) * nDindex%nDend(i) + nDval(i)
        not_out_of_range = not_out_of_range .AND. (nDval(i) <= nDindex%nDend(i))
        !write(out_unit,*) 'ib',ib,not_out_of_range
      END DO

      IF (.NOT. not_out_of_range) THEN
        nDI = nDindex%Max_nDI + 1
      ELSE
        nDI = ib
      END IF

    CASE (-1)
      ib = nDval(nDindex%ndim)
      not_out_of_range = (nDval(nDindex%ndim) <= nDindex%nDend(nDindex%ndim))
      !write(out_unit,*) 'ib',ib,not_out_of_range
      DO i=nDindex%ndim-1,1,-1
        ib = (ib-1) * nDindex%nDend(i) + nDval(i)
        not_out_of_range = not_out_of_range .AND. (nDval(i) <= nDindex%nDend(i))
        !write(out_unit,*) 'ib',ib,not_out_of_range
      END DO

      IF (.NOT. not_out_of_range) THEN
        nDI = nDindex%Max_nDI + 1
      ELSE
        nDI = ib
      END IF

    CASE (2,-3,3,-4,4,-5,5)

      CALL init_nDval_OF_nDindex(nDindex,nDval_tmp)
      DO ib=1,nDindex%Max_nDI
        CALL ADD_ONE_TO_nDindex(nDindex,nDval_tmp)
        IF (all(nDval_tmp == nDval)) EXIT
      END DO
      nDI = ib


    CASE DEFAULT
      write(out_unit,*) ' ERROR in calc_nDI'
      write(out_unit,*) ' You cannot use type_OF_nDindex/=1 with packed=F'
      write(out_unit,*) '           OR'
      write(out_unit,*) ' Not yet this type_OF_nDindex :',nDindex%type_OF_nDindex
      write(out_unit,*) ' CHECK the fortran source!'
      STOP
    END SELECT

  END IF

!-------------------------------------------------------
  IF (debug) THEN
    write(out_unit,*) '  nDI ',nDI
    write(out_unit,*) 'END ',name_sub
    flush(out_unit)
  END IF
!-------------------------------------------------------

  END SUBROUTINE calc_nDI_old
!     =================================================================
!      Calculation of the multidimensional index Norm
!         Sum_i nDval(i)*nDweight(i)
!             or
!         Sum_i nDval(i)
!     =================================================================
      FUNCTION calc_Norm_OF_nDI(nDindex,nDI) result(Norm)

        TYPE (Type_nDindex)        :: nDindex
        integer                    :: nDI

        real(kind=Rkind)           :: Norm


        integer                    :: nDval(nDindex%ndim)

        IF (nDindex%packed_done) THEN
          Norm = nDindex%Tab_Norm(nDI)
        ELSE
          CALL calc_nDindex(nDindex,nDI,nDval)
          Norm = calc_Norm_OF_nDval(nDval,nDindex)
        END IF

      END FUNCTION calc_Norm_OF_nDI
      FUNCTION calc_L_OF_nDI(nDindex,nDI) result(L)

        TYPE (Type_nDindex)        :: nDindex
        integer                    :: nDI,L

        integer                    :: nDval(nDindex%ndim)



        IF (nDindex%packed_done) THEN
          L = nDindex%Tab_L(nDI)
        ELSE
          CALL calc_nDindex(nDindex,nDI,nDval)
          L = calc_L_OF_nDval(nDval,nDindex)
        END IF

      END FUNCTION calc_L_OF_nDI

      !!@description: TODO
      !!@param: TODO
      FUNCTION calc_Norm_OF_nDval(nDval,nDindex,err_sub)

        TYPE (Type_nDindex)                              :: nDindex
        integer                                          :: nDval(:)
        integer,               intent(inout), optional   :: err_sub

        real(kind=Rkind)           :: Norm,calc_Norm_OF_nDval
        integer                    :: i,iNorm
        logical                    :: with_Tab_nDNorm
        logical                    :: with_Tab_i_TO_l

        IF (present(err_sub)) err_sub = 0

        IF (allocated(nDindex%Tab_nDNorm)) THEN
          with_Tab_nDNorm = allocated(nDindex%Tab_nDNorm(1)%vec)
        ELSE
          with_Tab_nDNorm = .FALSE.
        END IF

        IF (allocated(nDindex%Tab_i_TO_l)) THEN
          with_Tab_i_TO_l = allocated(nDindex%Tab_i_TO_l(1)%vec)
        ELSE
          with_Tab_i_TO_l = .FALSE.
        END IF

        !write(out_unit,*) 'calc_Norm_OF_nDval,with_Tab_nDNorm',with_Tab_nDNorm
        IF (with_Tab_nDNorm) THEN

          Norm = ZERO
          DO i=1,nDindex%ndim
            IF (nDval(i) > get_Size(nDindex%Tab_nDNorm(i))) THEN
             write(out_unit,*) ' ERROR in calc_Norm_OF_nDval'
             write(out_unit,*) ' nDval(i) > size of nDindex%Tab_nDNorm(i)'
             write(out_unit,*) ' i,nDval(i),nb_var_vec',i,nDval(i),get_Size(nDindex%Tab_nDNorm(i))
             write(out_unit,*) ' CHECK the fortran source !!'
             IF (present(err_sub)) THEN
               Norm               = nDindex%MaxNorm + ONE
               calc_Norm_OF_nDval = Norm
               err_sub            = err_nDval
               RETURN
             ELSE
               STOP
             END IF

           END IF
           Norm = Norm + nDindex%Tab_nDNorm(i)%vec(nDval(i))
          END DO
          calc_Norm_OF_nDval = Norm
        ELSE IF (with_Tab_i_TO_l) THEN

          iNorm = 0
          DO i=1,nDindex%ndim
            IF (nDval(i) > get_Size(nDindex%Tab_i_TO_l(i))) THEN
             write(out_unit,*) ' ERROR in calc_Norm_OF_nDval'
             write(out_unit,*) ' nDval(i) > size of nDindex%Tab_i_TO_l(i)'
             write(out_unit,*) ' i,nDval(i),size:',i,nDval(i),get_Size(nDindex%Tab_i_TO_l(i))
             write(out_unit,*) ' CHECK the fortran source !!'
             IF (present(err_sub)) THEN
               Norm               = nDindex%MaxNorm + ONE
               calc_Norm_OF_nDval = Norm
               err_sub            = err_nDval
               RETURN
             ELSE
               STOP
             END IF

           END IF
            iNorm = iNorm + nDindex%Tab_i_TO_l(i)%vec(nDval(i))
          END DO
          calc_Norm_OF_nDval = real(iNorm,kind=Rkind)
        ELSE
          calc_Norm_OF_nDval = sum(real(nDval-nDindex%nDinit,kind=Rkind)*nDindex%nDweight)
        END IF

      END FUNCTION calc_Norm_OF_nDval

      FUNCTION calc_L_OF_nDval(nDval,nDindex,err_sub)

        integer :: calc_L_OF_nDval
        TYPE (Type_nDindex),intent(in) :: nDindex
        integer                        :: nDval(:)
        integer,            intent(inout), optional     :: err_sub

        real(kind=Rkind)           :: Norm
        integer                    :: i,iNorm
        logical                    :: with_Tab_nDNorm
        logical                    :: with_Tab_i_TO_l


        IF (present(err_sub)) err_sub = 0

        IF (allocated(nDindex%Tab_nDNorm)) THEN
          with_Tab_nDNorm = allocated(nDindex%Tab_nDNorm(1)%vec)
        ELSE
          with_Tab_nDNorm = .FALSE.
        END IF

        IF (allocated(nDindex%Tab_i_TO_l)) THEN
          with_Tab_i_TO_l = allocated(nDindex%Tab_i_TO_l(1)%vec)
        ELSE
          with_Tab_i_TO_l = .FALSE.
        END IF

        !write(out_unit,*) 'calc_Norm_OF_nDval,with_Tab_nDNorm',with_Tab_nDNorm
        IF (with_Tab_nDNorm) THEN

          Norm = ZERO
          DO i=1,nDindex%ndim
            IF (nDval(i) > get_size(nDindex%Tab_nDNorm(i))) THEN
             write(out_unit,*) ' ERROR in calc_L_OF_nDval'
             write(out_unit,*) ' ndval(:)',ndval
             write(out_unit,*) ' nDval(i) > size of nDindex%Tab_nDNorm(i)'
             write(out_unit,*) ' i,nDval(i),nb_var_vec',i,nDval(i),get_size(nDindex%Tab_nDNorm(i))
             write(out_unit,*) ' CHECK the fortran source !!'
             IF (present(err_sub)) THEN
               calc_L_OF_nDval = nDindex%Lmax+1
               err_sub         = err_nDval
               RETURN
             ELSE
               STOP
             END IF

           END IF
           Norm = Norm + nDindex%Tab_nDNorm(i)%vec(nDval(i))
          END DO
          calc_L_OF_nDval = int(Norm)
        ELSE IF (with_Tab_i_TO_l) THEN
          iNorm = 0
          DO i=1,nDindex%ndim
            IF (nDval(i) > get_Size(nDindex%Tab_i_TO_l(i))) THEN
             write(out_unit,*) ' ERROR in calc_L_OF_nDval'
             write(out_unit,*) ' ndval(:)',ndval
             write(out_unit,*) ' nDval(i) > size of nDindex%Tab_i_TO_l(i)'
             write(out_unit,*) ' i,nDval(i),size',i,nDval(i),get_Size(nDindex%Tab_i_TO_l(i))
             write(out_unit,*) ' CHECK the fortran source !!'
             IF (present(err_sub)) THEN
               calc_L_OF_nDval = nDindex%Lmax+1
               err_sub         = err_nDval
               RETURN
             ELSE
               STOP
             END IF

           END IF
            iNorm = iNorm + nDindex%Tab_i_TO_l(i)%vec(nDval(i))
          END DO
          calc_L_OF_nDval = iNorm
        ELSE
          calc_L_OF_nDval = int(sum(real(nDval-nDindex%nDinit,kind=Rkind)*nDindex%nDweight))
        END IF

      END FUNCTION calc_L_OF_nDval

      SUBROUTINE calc_Max_nDI(nDindex)

        TYPE (Type_nDindex)        :: nDindex


        integer :: i


        character (len=*), parameter :: name_sub='calc_Max_nDI'

        SELECT CASE (nDindex%type_OF_nDindex)
        CASE (0)
          CALL calc_Max_nDI_type0(nDindex)

        CASE (1,-1)
          nDindex%Max_nDI = 1
          DO i=1,nDindex%ndim
            nDindex%Max_nDI = nDindex%Max_nDI * nDindex%nDsize(i)
            IF (nDindex%Max_nDI > 10**9) THEN
              write(out_unit,*) ' ERROR in ',name_sub
              write(out_unit,*) ' nDindex%Max_nDI will be too large'
              write(out_unit,*) ' At index',i,'Max_nDI =',nDindex%Max_nDI
              write(out_unit,*) ' we cannot use direct product!'
              write(out_unit,*) ' type_OF_nDindex:',nDindex%type_OF_nDindex
              write(out_unit,*) ' try type0 !!!'
              write(out_unit,*) ' Check the fortran source!!'
              STOP
            END IF
          END DO

        CASE (2)
          CALL calc_Max_nDI_type2(nDindex)

        CASE (5)
          CALL calc_Max_nDI_type5p(nDindex)
        CASE (-5)
          CALL calc_Max_nDI_type5m(nDindex)

        CASE DEFAULT
          write(out_unit,*) ' ERROR in ',name_sub
          write(out_unit,*) 'type_OF_nDindex',nDindex%type_OF_nDindex
          STOP
        END SELECT

      END SUBROUTINE calc_Max_nDI

      SUBROUTINE calc_Max_nDI_type2(nDindex)

        TYPE (Type_nDindex)        :: nDindex



        logical :: test
        integer :: i,nb_Coupling
        integer :: nDval(nDindex%ndim)
        real (kind=Rkind) :: Norm

        ! first the number of points
        nDindex%Max_nDI     = 0
        nDval(:)            = nDindex%nDinit(:)
        nDval(nDindex%ndim) = nDval(nDindex%ndim) - 1
        DO
          nDval(nDindex%ndim) = nDval(nDindex%ndim) + 1
          DO i=nDindex%ndim,1,-1
            test = .TRUE.
            IF (nDval(i) <= nDindex%nDend(i)) THEN
              Norm = calc_Norm_OF_nDval(nDval,nDindex)
              nb_Coupling = count((nDval-nDindex%nDinit) > 0)
              test = Norm > nDindex%MaxNorm .OR.                        &
                     Norm < nDindex%MinNorm .OR.                        &
                     nb_Coupling > nDindex%MaxCoupling .OR.             &
                     nb_Coupling < nDindex%MinCoupling
            END IF
            IF (test) THEN
              nDval(i) = nDindex%nDinit(i)
              IF (i>1) nDval(i-1) = nDval(i-1) + 1
            ELSE
              nDindex%Max_nDI = nDindex%Max_nDI + 1
              !write(out_unit,*) 'nDI,nDval,Norm',nDindex%Max_nDI,nDval,Norm
              !flush(out_unit)
              EXIT
            END IF
          END DO
          IF (test) EXIT
          !write(out_unit,*) 'nDI,nDval',Max_nDI,nDval
        END DO
      END SUBROUTINE calc_Max_nDI_type2

      SUBROUTINE calc_Max_nDI_type5p(nDindex)

        TYPE (Type_nDindex)        :: nDindex

        logical :: In_the_list
        integer :: nDval(nDindex%ndim)

        nDindex%Max_nDI     = 0

        CALL init_nDval_OF_nDindex(nDindex,nDval)
        DO
          CALL ADD_ONE_TO_nDindex_type5p(nDval,nDindex,In_the_list)

          IF (.NOT. In_the_list) EXIT
          nDindex%Max_nDI     = nDindex%Max_nDI + 1
          !write(out_unit,*) 'nDI,nDval',nDindex%Max_nDI,nDval
        END DO

      END SUBROUTINE calc_Max_nDI_type5p
      SUBROUTINE calc_Max_nDI_type5m(nDindex)

        TYPE (Type_nDindex)        :: nDindex

        logical :: In_the_list
        integer :: nDval(nDindex%ndim)

        nDindex%Max_nDI     = 0

        CALL init_nDval_OF_nDindex(nDindex,nDval)
        DO
          CALL ADD_ONE_TO_nDindex_type5m(nDval,nDindex,In_the_list)
          IF (.NOT. In_the_list) EXIT
          nDindex%Max_nDI     = nDindex%Max_nDI + 1
          !write(out_unit,*) 'nDI,nDval',nDindex%Max_nDI,nDval
        END DO

      END SUBROUTINE calc_Max_nDI_type5m

      SUBROUTINE calc_Max_nDI_type0(nDindex)

        TYPE (Type_nDindex)        :: nDindex



        logical :: test
        integer :: i,nb_Coupling
        integer :: nDval(nDindex%ndim)
        real (kind=Rkind) :: Norm

        ! first the number of points
        nDindex%Max_nDI     = 0
        nDval(:)            = nDindex%nDinit(:)
        nDval(nDindex%ndim) = nDval(nDindex%ndim) - 1
        DO
          nDval(nDindex%ndim) = nDval(nDindex%ndim) + 1
          DO i=nDindex%ndim,1,-1
            test = .TRUE.
            IF (nDval(i) <= nDindex%nDend(i)) THEN
              Norm = calc_Norm_OF_nDval(nDval,nDindex)
              test = Norm > nDindex%MaxNorm
            END IF
            IF (test) THEN
              nDval(i) = nDindex%nDinit(i)
              IF (i>1) nDval(i-1) = nDval(i-1) + 1
            ELSE
              nb_Coupling = count((nDval-nDindex%nDinit) > 0)
              !write(out_unit,*) 'nb_coupling',nDval,':',nb_Coupling

              IF (Norm >= nDindex%MinNorm .AND.                         &
                  nb_Coupling <= nDindex%MaxCoupling .AND.              &
                  nb_Coupling >= nDindex%MinCoupling) THEN
                nDindex%Max_nDI = nDindex%Max_nDI + 1
                !write(out_unit,*) 'nDI,nDval,Norm',nDindex%Max_nDI,nDval,Norm
                flush(out_unit)
              END IF
              EXIT
            END IF
          END DO
          IF (test) EXIT
          !write(out_unit,*) 'nDI',nDindex%Max_nDI
        END DO
      END SUBROUTINE calc_Max_nDI_type0

!======================================================================
      !!@description: TODO
      !!@param: TODO
      SUBROUTINE Write_nDindex(nDindex,name_info)
        IMPLICIT NONE
        TYPE (Type_nDindex) :: nDindex
        character (len=*), optional  :: name_info

        integer :: i,err_sub,nDval(nDindex%ndim)
        character (len=Name_longlen) :: name_info_loc


        IF (present(name_info)) THEN
          name_info_loc = trim(adjustl(name_info)) // ': '
        ELSE
          name_info_loc = ''
        END IF

        write(out_unit,*) trim(name_info_loc),'==============================================='
        write(out_unit,*) trim(name_info_loc),'Write_nDindex'

        write(out_unit,*) trim(name_info_loc),'alloc',nDindex%alloc
        write(out_unit,*) trim(name_info_loc),'init',nDindex%init
        write(out_unit,*) trim(name_info_loc),'Write_Tab',nDindex%Write_Tab
        write(out_unit,*) trim(name_info_loc),'type_OF_nDindex',nDindex%type_OF_nDindex
        write(out_unit,*) trim(name_info_loc),'MaxCoupling',nDindex%MaxCoupling
        write(out_unit,*) trim(name_info_loc),'MinCoupling',nDindex%MinCoupling

        write(out_unit,*) trim(name_info_loc),'MaxNorm',nDindex%MaxNorm
        write(out_unit,*) trim(name_info_loc),'MinNorm',nDindex%MinNorm

        write(out_unit,*) trim(name_info_loc),'Lmin',nDindex%Lmin
        write(out_unit,*) trim(name_info_loc),'Lmax',nDindex%Lmax
        write(out_unit,*) trim(name_info_loc),'With_L',nDindex%With_L


        write(out_unit,*) trim(name_info_loc),'ndim',nDindex%ndim
        write(out_unit,*) trim(name_info_loc),'Max_nDI',nDindex%Max_nDI

        IF (allocated(nDindex%nDinit))                                 &
          write(out_unit,*) trim(name_info_loc),'nDinit',nDindex%nDinit
        IF (allocated(nDindex%nDend))                                  &
          write(out_unit,*) trim(name_info_loc),'nDend',nDindex%nDend
        IF (allocated(nDindex%nDsize))                                 &
          write(out_unit,*) trim(name_info_loc),'nDsize',nDindex%nDsize
        IF (allocated(nDindex%nDweight))                               &
          write(out_unit,*) trim(name_info_loc),'nDweight',nDindex%nDweight

        write(out_unit,*) trim(name_info_loc),'packed,packed_done',    &
                                      nDindex%packed,nDindex%packed_done

        IF (allocated(nDindex%skip_li)) THEN
          DO i=1,nDindex%ndim
             write(out_unit,*) i,'skip_li(:,i)',nDindex%skip_li(:,i)
          END DO
        END IF

        write(out_unit,*) trim(name_info_loc),'Tab_i_TO_l'
        IF (allocated(nDindex%Tab_i_TO_l)) THEN
          DO i=1,nDindex%ndim
             CALL Write_IntVec(nDindex%Tab_i_TO_l(i))
          END DO
        END IF

        IF (allocated(nDindex%Tab_L)) THEN
          DO i=1,nDindex%Max_nDI
             write(out_unit,*) trim(name_info_loc),'nDI',i,':',        &
                                            ' L:',nDindex%Tab_L(i)
          END DO
        END IF

        IF (allocated(nDindex%Tab_nDval) .AND.                         &
            allocated(nDindex%Tab_Norm) ) THEN
          DO i=1,nDindex%Max_nDI
             write(out_unit,*) trim(name_info_loc),'nDI,nDval',i,':',  &
                     nDindex%Tab_nDval(:,i),' Norm:',nDindex%Tab_Norm(i)
          END DO
        END IF
        IF (allocated(nDindex%Tab_nDval) .AND.                         &
            .NOT. allocated(nDindex%Tab_Norm) ) THEN
          DO i=1,nDindex%Max_nDI
             write(out_unit,*) trim(name_info_loc),'nDI,nDval',i,':',  &
                     nDindex%Tab_nDval(:,i)
          END DO
        END IF
        IF (.NOT. allocated(nDindex%Tab_nDval) .AND.                   &
            allocated(nDindex%Tab_Norm) ) THEN
          DO i=1,nDindex%Max_nDI
             write(out_unit,*) trim(name_info_loc),'nDI,Norm',i,':',   &
                         nDindex%Tab_Norm(i)
          END DO
        END IF

        IF (.NOT. allocated(nDindex%Tab_nDval)) THEN
          DO i=1,nDindex%Max_nDI
            CALL calc_nDindex(nDindex,i,nDval,err_sub)
            IF (err_sub /= 0) STOP ' ERROR with calc_nDindex!!'
            write(out_unit,*) trim(name_info_loc),'nDI,nDval',i,':',nDval(:)
          END DO
        END IF

        IF (allocated(nDindex%Tab_nDNorm)) THEN
          write(out_unit,*) trim(name_info_loc),'Tab_nDNorm:'
          DO i=1,nDindex%ndim
            write(out_unit,*) trim(name_info_loc),'Tab_nDNorm(i):',i,  &
                                             nDindex%Tab_nDNorm(i)%vec(:)
          END DO
        END IF

        IF (allocated(nDindex%Tab_DInd)) CALL Write_Tab_nDInd(nDindex%Tab_DInd)


        write(out_unit,*) trim(name_info_loc),'==============================================='


      END SUBROUTINE Write_nDindex

END MODULE mod_nDindex
