#' Default feature definitions
#'
#' @param df dataframe which has columns curve_id and 0,1,...,100, and (optionally) ofs (opposite foot strike time) for pelvis features.
#' @details For help on writing your own feature definition see here.
#' @return same as df but augmented with extra colunn(s) indicating whether the feature is detected
#' @examples
#' .dtIntoe(kinematics)
#' @rdname definitions
#' @export .dtAIR
#' @export .dtDblBump
#' @export .dtDecDF
#' @export .dtDecFxLoading
#' @export .dtDecHipFxIC
#' @export .dtDecPelTilt
#' @export .dtDecPelTiltIncROM
#' @export .dtDecPkKnFx
#' @export .dtDelayDecPkKnFx
#' @export .dtDelayIncPkKnFx
#' @export .dtDelayPkKnFx
#' @export .dtDesc2R
#' @export .dtDFSwg
#' @export .dtEPS
#' @export .dtExHipAbd
#' @export .dtExHipAbdSwg
#' @export .dtExHipAdd
#' @export .dtFtD
#' @export .dtFxMS
#' @export .dtH2R
#' @export .dtHipAddSt
#' @export .dtHipER
#' @export .dtHipExtDef
#' @export .dtHipHypFx
#' @export .dtHipIR
#' @export .dtHypExt
#' @export .dtIncDF
#' @export .dtIncFxIC
#' @export .dtIncFxICEarlyKnExt
#' @export .dtIncHipExtMS
#' @export .dtIncHipFx
#' @export .dtIncHipFxDecROM
#' @export .dtIncIRLateSt
#' @export .dtIncMaxDF
#' @export .dtIncPelROM
#' @export .dtIncPelRotROM
#' @export .dtIncPelTilt
#' @export .dtIncPelTiltIncROM
#' @export .dtIncPF
#' @export .dtIncPkKnFx
#' @export .dtIntoe
#' @export .dtIPP
#' @export .dtN1R
#' @export .dtOutoe
#' @export .dtPelElevDepr
#' @export .dtProRetraction
#' @export .dtRevROM
#' @export .dtS2R

definitions=function()NULL


#' @rdname definitions
## Foot-----------------------------------------------
## intoeing--------
.dtIntoe <- function(df) {
  df%>%
    filter(joint=="FootProgress"&plane=="tra")%>%
    unistat(`0`:`60`,.f=mean,.dir=">")%>%
    finish(featurename = "Intoe",fname = "In")
}
#' @rdname definitions
## outoeing------------------
.dtOutoe <- function(df) {
  df%>%
    filter(joint=="FootProgress"&plane=="tra")%>%
    unistat(`0`:`60`,.f=mean,.dir="<")%>%
    finish(featurename = "Outoe",fname = "Out")
}
#' @rdname definitions
## Ext foot prog in swing-----------------
.dtEPS=function(df){
  df%>%
    filter(joint=="FootProgress"&plane=="tra")%>%
    unistat(`60`:`100`,.f=.ROM,.dir=">",k=2)%>%
    corr(`0`:`100`,TC=.tc()$ett)%>%
    finish(featurename = "External Foot Progression (Wave) in Swing only.",fname = "EPS")
}
#' @rdname definitions
## DelayIncPkKnFx---------------
.dtDelayIncPkKnFx=function(df){
  df%>%
    filter(joint=="Knee"&plane=="sag")%>%
    unistat(`60`:`100`,.f=max,.dir=">",k=2)%>%
    custom(`60`:`100`,cond="t[which.max(angle)]>75")%>%
    finish(critDesc = "peak occurs after t=75[2sd] and >2sd",featurename = "Delayed + Increased Peak Knee Flexion",fname = "DlyPkKFxat")
}
#' @rdname definitions
## DecPkKnFx----------------------
.dtDecPkKnFx=function(df){
  df%>%
    filter(joint=="Knee"&plane=="sag")%>%
    unistat(`60`:`100`,.f=max,.dir="<",k=2)%>%
    finish(critDesc = "peak(60:100)<2sd",featurename = "Decreased Peak Knee Flexion",fname = "PkKFxdec")
}

#' @rdname definitions
##  DelayDecPkKnFx
.dtDelayDecPkKnFx=function(df){
  df%>%
    filter(joint=="Knee"&plane=="sag")%>%
    unistat(`60`:`100`,.f=max,.dir="<",k=2)%>%
    custom(`60`:`100`,cond="t[which.max(angle)]>75")%>%
    finish(critDesc = "peak occurs after t=75[2sd] and <2sd",featurename = "Delayed + Decreased Peak Knee Flexion",fname = "DlyPkKFxdec")
}

#' @rdname definitions
## DecFxLoading------------
.dtDecFxLoading=function(df){
  df%>%
    filter(joint=="Knee"&plane=="sag")%>%
    unistat(`0`:`20`,.f=mean,.dir="<")%>%
    finish(critDesc = "mean(0:20)<1sd",featurename = "Reduced flexion @ loading",fname = "KFxatLdec")
}


## Ankle-----------------------------------------------
## horiztonal 2nd rocker----------
#' @rdname definitions
.dtH2R=function(df){
  df%>%
    filter(joint=="Ankle"&plane=="sag")%>%
    unistat(`20`:`45`,.f=.ROM,.dir="<",.c=5)%>%
    unistat(`20`:`45`,.f=.aslope,.dir="<",.c=0.1)%>%
    unistat(`45`,.f=c,.dir="<",.c=0)%>%
    finish(featurename = "Horizontal 2nd Rocker",fname = "H2R")
}

#' @rdname definitions
## short 2nd rocker-----------------------------
.dtS2R=function(df){
  df%>%
    filter(joint=="Ankle"&plane=="sag")%>%
    unistat(`5`:`20`,.f = .npeak,.dir = ">",.c = 0)%>%
    unistat(`20`:`45`,.f=.slope,.dir="<",.c=0)%>%
    corr(`0`:`60`,TC = .tc()$s2r)%>%
    finish(featurename = "Short 2nd Rocker",fname = "S2R")
}
#' @rdname definitions
## reduced dorsiflexion --------
.dtDecDF=function(df){
  df%>%
    filter(joint=="Ankle"&plane=="sag")%>%
    unistat(`0`:`50`,.f=mean,.dir="<")%>%
    finish(featurename = "Reduced Dorsiflexion",fname = "DFdec")
}
#' @rdname definitions
## desc2ndRocker-----------------------
.dtDesc2R=function(df){
  df%>%
    filter(joint=="Ankle"&plane=="sag")%>%
    unistat(`20`,`45`,.f=diff,.dir="<",.c=-5)%>%
    unistat(`45`,.f=c,.dir="<",.c=0)%>%
    finish(featurename = "Descending 2nd Rocker",fname = "Desc2R")
}
#' @rdname definitions
## equinus--------------------------
.dtIncPF=function(df){
  df%>%
    filter(joint=="Ankle"&plane=="sag")%>%
    unistat(`20`,`45`,.f=mean,.dir="<")%>%
    finish(featurename = "Increased Plantarflexion",fname = "PFinc")
}
#' @rdname definitions
## Calcaneus--------------------------------
.dtIncMaxDF=function(df){
  df%>%
    filter(joint=="Ankle"&plane=="sag")%>%
    unistat(`0`:`60`,.f=max,.dir=">",k=2)%>%
    finish(featurename = "Increased Max. Dorsiflexion",fname = "MaxDFinc")
}
#' @rdname definitions
## increased dorsiflexion----------------------
.dtIncDF=function(df){
  df%>%
    filter(joint=="Ankle"&plane=="sag")%>%
    unistat(`20`:`45`,.f=mean,.dir=">")%>%
    finish(featurename = "Increased Dorsiflexion",fname = "DFinc")
}

#' @rdname definitions
## insufficient pre-positioning-------------
.dtIPP=function(df){
  df%>%
    filter(joint=="Ankle"&plane=="sag")%>%
    unistat(`100`,.f=c,.dir="<",k=2)%>%
    finish(featurename = "Insufficient pre-positioning",fname = "iPP")
}
#' @rdname definitions
## no 1st rocker-----------------------
.dtN1R=function(df){
  df%>%
    filter(joint=="Ankle"&plane=="sag")%>%
    unistat(`0`,`1`,.f=diff,.dir=">",.c=0)%>%
    finish(featurename = "No 1st Rocker",fname = "N1R")
}
#' @rdname definitions
## foot drop-----------------
.dtFtD=function(df){
  df%>%
    filter(joint=="Ankle"&plane=="sag")%>%
    unistat(`80`:`100`,.f=mean,.dir="<")%>%
    finish(featurename = "Foot Drop",fname = "FtD")
}

#' @rdname definitions
## dorsiflexion in swing----------------
.dtDFSwg=function(df){
  df%>%
    filter(joint=="Ankle"&plane=="sag")%>%
    unistat(`60`:`100`,.f=mean,.dir=">")%>%
    finish(featurename = "Dorsiflexion in Swing",fname = "DFatSwg")
}

#' @rdname definitions
## Ankle tra-------------------------------------------------
## internal rotation---------------
.dtAIR=function(df){
  df%>%
    filter(joint=="Ankle"&plane=="tra")%>%
    unistat(`0`:`60`,.f=mean,.dir=">")%>%
    finish(featurename = "Ankle Internal Rotation",fname = "AIR")
}

#' @rdname definitions
## Knee sag-------------------------------------------------
## IncFxIC-----------------
.dtIncFxIC=function(df){
  df%>%
    filter(joint=="Knee"&plane=="sag")%>%
    unistat(`0`,.f=c,.dir=">",k=2)%>%
    finish(featurename = "Increased flexion @ IC",fname = "KFxatICinc")
}
#' @rdname definitions
## IncFxICEarlyKnExt------------------
.dtIncFxICEarlyKnExt=function(df){
  df%>%
    filter(joint=="Knee"&plane=="sag")%>%
    unistat(`0`,.f=c,.dir=">",k=2)%>%
    unistat(`10`:`25`,.f=min,.dir="<",.k=-1)%>%
    unistat(`10`:`25`,.f=.npit,.dir=">",.c=0)%>%
    # custom(`0`:`25`,cond = "abs(angle[1]-min(angle))>10")%>%
    unistat(`0`:`25`,.f=function(x)abs(x[1]-min(x)),.dir=">",.c=10)%>%
    finish(featurename = "Increased flexion @ IC+\n early knee extension",fname = "KFxatICincAndEE")
}
#' @rdname definitions
## hypExt------------
.dtHypExt=function(df){
  df%>%
    filter(joint=="Knee"&plane=="sag")%>%
    unistat(`20`:`45`,.f=mean,.dir="<")%>%
    finish(featurename = "Knee Hyperextension",fname = "HypE")
}

#' @rdname definitions
## FxInMS-----------------
.dtFxMS=function(df){
  df%>%
    filter(joint=="Knee"&plane=="sag")%>%
    unistat(`20`:`45`,.f=mean,.dir=">")%>%
    finish(featurename = "Knee Flexion in mid stance",fname = "KFxatMSinc")
}
#' @rdname definitions
## IncPkKnFx-----------------
.dtIncPkKnFx=function(df){
  df%>%
    filter(joint=="Knee"&plane=="sag")%>%
    unistat(`60`:`100`,.f=max,.dir=">",k=2)%>%
    finish(featurename = "Increased Peak Knee Flexion",fname = "PkKFxat")
}

#' @rdname definitions
## DelayPkKnFx----------------------
.dtDelayPkKnFx=function(df){
  df%>%
    filter(joint=="Knee"&plane=="sag")%>%
    custom(`60`:`100`,cond="t[which.max(angle)]>75")%>%
    finish(featurename = "Delayed Peak Knee Flexion",fname = "DlyPkKFx")
}

#' @rdname definitions
## DelayIncPkKnFx---------------
.dtDelayIncPkKnFx=function(df){
  df%>%
    filter(joint=="Knee"&plane=="sag")%>%
    unistat(`60`:`100`,.f=max,.dir=">",k=2)%>%
    custom(`60`:`100`,cond="t[which.max(angle)]>75")%>%
    finish(featurename = "Delayed + Increased Peak Knee Flexion",fname = "DlyPkKFxat")
}
#' @rdname definitions
## DecPkKnFx----------------------
.dtDecPkKnFx=function(df){
  df%>%
    filter(joint=="Knee"&plane=="sag")%>%
    unistat(`60`:`100`,.f=max,.dir="<",k=2)%>%
    finish(featurename = "Decreased Peak Knee Flexion",fname = "PkKFxdec")
}

#' @rdname definitions
##  DelayDecPkKnFx
.dtDelayDecPkKnFx=function(df){
  df%>%
    filter(joint=="Knee"&plane=="sag")%>%
    unistat(`60`:`100`,.f=max,.dir="<",k=2)%>%
    custom(`60`:`100`,cond="t[which.max(angle)]>75")%>%
    finish(featurename = "Delayed + Decreased Peak Knee Flexion",fname = "DlyPkKFxdec")
}

#' @rdname definitions
## DecFxLoading------------
.dtDecFxLoading=function(df){
  df%>%
    filter(joint=="Knee"&plane=="sag")%>%
    unistat(`0`:`20`,.f=mean,.dir="<")%>%
    finish(featurename = "Reduced flexion @ loading",fname = "KFxatLdec")
}
#' @rdname definitions
## Hip--------------------------------------------------
## HipExtDef---------------
.dtHipExtDef=function(df){
  df%>%
    filter(joint=="Hip"&plane=="sag")%>%
    unistat(`0`:`60`,.f=mean,.dir=">")%>%
    unistat(`0`:`100`,.f=.ROM,.dir="<",k=2)%>%
    finish(featurename = "Hip extension deficit",fname = "HExtdec")
}

#' @rdname definitions
## IncHipFx----------------
.dtIncHipFx=function(df){
  df%>%
    filter(joint=="Hip"&plane=="sag")%>%
    unistat(`0`:`100`,.f=mean,.dir=">")%>%
    unistat(`0`:`100`,.f=min,.dir=">",.c=0)%>%
    finish(featurename = "Increased Hip Flexion",fname = "HFxinc")
}
#' @rdname definitions
## HipHyperFx--------------
.dtHipHypFx=function(df){
  df%>%
    filter(joint=="Hip"&plane=="sag")%>%
    unistat(`60`:`100`,.f=max,.dir=">",k=2)%>%
    unistat(`0`:`60`,.f=mean,.dir="%w%")%>%
    finish(featurename = "Hip hyper-flexion",fname = "HypHFx")
}

#' @rdname definitions
## IncHipExtAtMS----------------
.dtIncHipExtMS=function(df){
  df%>%
    filter(joint=="Hip"&plane=="sag")%>%
    unistat(`20`:`45`,.f=mean,.dir="<")%>%
    finish(featurename = "Increased Hip extension @ MidStance",fname = "HExtatMSinc")
}
#' @rdname definitions
## IncHipFXDecROM---------------------
.dtIncHipFxDecROM=function(df){
  df%>%
    filter(joint=="Hip"&plane=="sag")%>%
    unistat(`0`:`100`,.f=mean,.dir=">")%>%
    unistat(`0`:`100`,.f=.ROM,.dir="<",k=2)%>%
    finish(featurename = "Increased Hip flexion+\n decreased ROM",fname = "HFxincAndROMdec")
}
#' @rdname definitions
## DecHipFxIC------------------
.dtDecHipFxIC=function(df){
  df%>%
    filter(joint=="Hip"&plane=="sag")%>%
    unistat(`0`,.f=c,.dir="<",k=2)%>%
    finish(featurename = "Decreased hip flexion @ IC",fname = "HFxatICdec")
}

#' @rdname definitions
## HIp cor----------------------------------------------------
## ExHipAbdSwg-------------
.dtExHipAbdSwg=function(df){
  df%>%
    filter(joint=="Hip"&plane=="cor")%>%
    unistat(`60`:`100`,.f=mean,.dir="<")%>%
    finish(featurename = "Excessive Hip abduction in Swing",fname = "AbdatSwn")
}
#' @rdname definitions
## ExhipAdd---------------
.dtExHipAdd=function(df){
  df%>%
    filter(joint=="Hip"&plane=="cor")%>%
    unistat(`0`:`60`,.f=mean,.dir=">")%>%
    unistat(`60`:`100`,.f=mean,.dir=">")%>%
    finish(featurename = "Excessive Hip adduction",fname = "Add")
}

#' @rdname definitions
## ExhipAbd---------------
.dtExHipAbd=function(df){
  df%>%
    filter(joint=="Hip"&plane=="cor")%>%
    unistat(`0`:`60`,.f=mean,.dir="<")%>%
    unistat(`60`:`100`,.f=mean,.dir="<")%>%
    finish(featurename = "Excessive Hip abduction",fname = "Abd")

}

#' @rdname definitions
## HipAddStance-------------
.dtHipAddSt=function(df){
  df%>%
    filter(joint=="Hip"&plane=="cor")%>%
    unistat(`0`:`60`,.f=mean,.dir=">")%>%
    finish(featurename = "Hip adduction @ stance",fname = "AddatStn")
}

#' @rdname definitions
## Hip transverse---------------------------------
## HipIR---------------
.dtHipIR=function(df){
  df%>%
    filter(joint=="Hip"&plane=="tra")%>%
    unistat(`0`:`100`,.f=mean,.dir=">")%>%
    finish(featurename = "Hip internal rotation",fname = "HIR")
}

#' @rdname definitions
## HipER---------------
.dtHipER=function(df){
  df%>%
    filter(joint=="Hip"&plane=="tra")%>%
    unistat(`0`:`100`,.f=mean,.dir="<")%>%
    finish(featurename = "Hip external rotation",fname = "HER")
}
#' @rdname definitions
## IncIRLS------------
.dtIncIRLateSt=function(df){
  df%>%
    filter(joint=="Hip"&plane=="tra")%>%
    unistat(`40`:`60`,.f=mean,.dir=">")%>%
    custom(`0`:`100`,cond="t[which.max(angle)]<70")%>%
    unistat(`20`:`80`,.f = .npit,.dir="<",.c=1)%>%
    finish(featurename = "Increased IR @ late stance",fname = "HIRatLSinc")

}

#' @rdname definitions
## Pelvis sag------------------------------------------------
## IncPelTilt--------------------
.dtIncPelTilt=function(df){
  df%>%
    filter(joint=="Pel"&plane=="sag")%>%
    unistat(`0`:`100`,.f=mean,.dir=">")%>%
    finish(featurename = "Increased pelvic tilt",fname = "Tiltinc")
}
#' @rdname definitions
## DecPelTilt--------------------
.dtDecPelTilt=function(df){
  df%>%
    filter(joint=="Pel"&plane=="sag")%>%
    unistat(`0`:`100`,.f=mean,.dir="<")%>%
    finish(featurename = "Decreased pelvic tilt",fname = "Tiltdec")
}
#' @rdname definitions
## DblBump-------------------
.dtDblBump=function(df){
  df%>%
    filter(joint=="Pel"&plane=="sag")%>%
    unistat(`0`:`100`,.f=.ROM,.dir=">",k=2)%>%
    unistat(`0`:`100`,.f=.period,.dir="==",.c=2)%>%
    corr(`0`:`100`,TC=.tc()$dblbump)%>%
    # dLR(`0`:`100`,.dir="<",.c="ROM",k=0.25)%>%
    finish(featurename = "Increased ROM (double bump)",fname = "DblB")
}
#' @rdname definitions
## IncPelTiltIncROM---------------
.dtIncPelTiltIncROM=function(df){
  df%>%
    filter(joint=="Pel"&plane=="sag")%>%
    unistat(`0`:`100`,.f=mean,.dir=">")%>%
    unistat(`0`:`100`,.f=.ROM,.dir=">",k=2)%>%
    finish(featurename = "Increased Pelvic Tilt + \n Increased ROM",fname = "TiltincAndROMinc")
}
#' @rdname definitions
## IncPelTiltIncROM---------------
.dtIncPelTiltIncROM=function(df){
  df%>%
    filter(joint=="Pel"&plane=="sag")%>%
    unistat(`0`:`100`,.f=mean,.dir=">")%>%
    unistat(`0`:`100`,.f=.ROM,.dir=">",k=2)%>%
    finish(featurename = "Increased Pelvic Tilt + \n Increased ROM",fname = "TiltincAndROMinc")
}

#' @rdname definitions
## DecPelTiltIncROM---------------
.dtDecPelTiltIncROM=function(df){
  df%>%
    filter(joint=="Pel"&plane=="sag")%>%
    unistat(`0`:`100`,.f=mean,.dir="<")%>%
    unistat(`0`:`100`,.f=.ROM,.dir=">",k=2)%>%
    finish(featurename = "Decreased Pelvic Tilt + \n Increased ROM",fname = "TiltdecAndROMinc")
}

## UniBumpAtPeakHipExt--------------
# .dtUniBump=function(df,.DblBump=DblBump){
#   df%>%
#     filter(joint=="Pel"&plane=="sag")%>%
#     unistat(`0`:`100`,.f=.ROM,.dir=">",k=2,critDesc = "Cl:ROM>2sd but not DblBump")%>%
#     anti_join(.DblBump%>%filter(DblB==1))%>%
#     finish(featurename = "Unilateral Bump",fname = "UniBump")
# }

#' @rdname definitions
## Pel cor------------------------------------------------------
## PelElevDepr---------------
.dtPelElevDepr=function(df){
  df%>%
    filter(joint=="Pel"&plane=="cor")%>%
    # PelR2L(ofs="ofs",flipped=T)%>%
    # dLR(`0`:`100`,.dir = ">",.c = 1.818387+2.204310)%>%
    unistat(`0`:`100`,.f=mean,.dir="%o%")%>%
    finish(featurename = "Pelvic elevation/depression",fname = "ElevOrDep")
}
#' @rdname definitions
## IncPelROM-----------------
.dtIncPelROM=function(df){
  df%>%
    filter(joint=="Pel"&plane=="cor")%>%
    unistat(`0`:`100`,.f=.ROM,.dir=">",k=2)%>%
    finish(featurename = "Increased pelvic ROM",fname = "PROMinc")
}
#' @rdname definitions
## Pel tra-------------------------------------------------
## ProRetraction--------------
.dtProRetraction=function(df){
  df%>%
    filter(joint=="Pel"&plane=="tra")%>%
    # dLR(`0`:`100`,.dir = ">",.c = 2.583670+3.042503)%>%
    unistat(`0`:`100`,.f=mean,.dir="%o%")%>%
    finish(featurename = "Pelvic Pro / Retraction",fname = "ProOrRe")
}

#' @rdname definitions
## IncPelROM-----------------
.dtIncPelRotROM=function(df){
  df%>%
    filter(joint=="Pel"&plane=="tra")%>%
    unistat(`0`:`100`,.f=.ROM,.dir=">",k=2)%>%
    finish(featurename = "Increased Pelvic Rotation ROM",fname = "PRotROMinc")
}

#' @rdname definitions
## RevROM----------------
.dtRevROM=function(df){
  df%>%
    filter(joint=="Pel"&plane=="tra")%>%
    corr(`0`:`100`,TC = .tc()$revrom)%>%
    finish(featurename = "Reversed ROM",fname = "PrevROM")
}
