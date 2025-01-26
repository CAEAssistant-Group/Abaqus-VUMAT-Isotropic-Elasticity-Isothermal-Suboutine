C      ######################################################################
C      #################      CAE Assistant Company          ################
C      ##############         CAEassistant-com              #############
C      ###########   Copy right by CAE Assistant Company    ###############
C      ######################################################################

C      ######################################################################
C      ######################################################################
C      CAE Assisitant Services: 
C      Toturial Packages,Consultancy,Articles,Q&A,Video Gallery,Online Course
C      ######################################################################
C      Need help with your project? 
C      You can get initial free consultation from (our website caeassistant com)
C      ###################################################################### 	
      subroutine vumat(
C Read only (unmodifiable)variables -
     1  nblock, ndir, nshr, nstatev, nfieldv, nprops, jInfoArray,
     2  stepTime, totalTime, dtArray, cmname, coordMp, charLength,
     3  props, density, strainInc, relSpinInc,
     4  tempOld, stretchOld, defgradOld, fieldOld,
     5  stressOld, stateOld, enerInternOld, enerInelasOld,
     6  tempNew, stretchNew, defgradNew, fieldNew,
C Write only (modifiable) variables -
     7  stressNew, stateNew, enerInternNew, enerInelasNew)
C
      include 'vaba_param.inc'
      parameter (i_info_AnnealFlag = 1, 
     *     i_info_Intpt    = 2, ! Integration station number
     *     i_info_layer  = 3, ! Layer number
     *     i_info_kspt   = 4, ! Section point number in current layer
     *     i_info_effModDefn = 5, ! =1 if Bulk/ShearMod need to be defined
     *     i_info_ElemNumStartLoc   = 6) ! Start loc of user element number
C
      dimension props(nprops), density(nblock), coordMp(nblock,*),
     1  charLength(nblock), dtArray(2*(nblock)+1), strainInc(nblock,ndir+nshr),
     2  relSpinInc(nblock,nshr), tempOld(nblock), 
     3  stretchOld(nblock,ndir+nshr),
     4  defgradOld(nblock,ndir+nshr+nshr),
     5  fieldOld(nblock,nfieldv), stressOld(nblock,ndir+nshr),
     6  stateOld(nblock,nstatev), enerInternOld(nblock),
     7  enerInelasOld(nblock), tempNew(nblock),
     8  stretchNew(nblock,ndir+nshr),
     8  defgradNew(nblock,ndir+nshr+nshr),
     9  fieldNew(nblock,nfieldv),
     1  stressNew(nblock,ndir+nshr), stateNew(nblock,nstatev),
     2  enerInternNew(nblock), enerInelasNew(nblock), jInfoArray(*)
C
      character*80 cmname
C
      pointer (ptrjElemNum, jElemNum)
      dimension jElemNum(nblock)
      
      REAL*8 zero, one, two, third, half
      REAL*8 E, Nu, EG2, ELAM, EG
      zero = 0.d0
      one = 1.d0
      two = 2.d0
      third = 1.d0 / 3.d0
      half = 0.5d0
C
      lAnneal = jInfoArray(i_info_AnnealFlag) 
      iLayer = jInfoArray(i_info_layer)
      kspt   = jInfoArray(i_info_kspt)
      intPt  = jInfoArray(i_info_Intpt)
      iUpdateEffMod = jInfoArray(i_info_effModDefn)
      iElemNumStartLoc = jInfoArray(i_info_ElemNumStartLoc)
      ptrjElemNum = loc(jInfoArray(iElemNumStartLoc))

C
	
C 	props(1) Young’s modulus
C 	props(2) Poisson’s ratio
C    
	E = props(1)
	Nu = props(2)
	EG2 = E / (one + nu)
	ELAM = Nu * EG2 / (one - two * Nu)
C
	do 100 km = 1, nblock
C	
	stressNew(km,1) = stressOld(km,1)
     * + EG2 * strainInc(km,1) + ELAM *
     *(strainInc(km,1) + strainInc(km,2) + strainInc(km,3))
      
      stressNew(km,2) = stressOld(km,2)
     * + EG2 * strainInc(km,2) + ELAM *
     *(strainInc(km,1) + strainInc(km,2) + strainInc(km,3))
      
      stressNew(km,3) = stressOld(km,3)
     * + EG2 * strainInc(km,3) + ELAM *
     *(strainInc(km,1) + strainInc(km,2) + strainInc(km,3))
      
	stressNew(km,4) = stressOld(km,4) + EG2 * strainInc(km,4)
      stressNew(km,5) = stressOld(km,5) + EG2 * strainInc(km,5)
      stressNew(km,6) = stressOld(km,6) + EG2 * strainInc(km,6)

  100 continue
C
	return
	end