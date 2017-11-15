library(gaitFeature)
context("detectAll")


test_that("makeFDict",{
  fD=makeFDict()
  expect_named(fD,c("fn","joint","plane","fname","featurename"))
})


test_that("detectAll",{
  D=detectAll(kinematics)
  fDict=makeFDict()
  expect_equal(nrow(D),nrow(kinematics))
  expect_equal(ncol(D),ncol(kinematics)+nrow(fDict))
})

test_that("resamp101",{
  x=kinematics%>%
    select(curve_id,`0`,`10`,`20`,`30`,`40`,`50`,`60`,`70`,`80`,`90`,`100`)
  y=resamp101(x,`0`:`100`,plot=F)
  expect_named(y%>%select(num_range("",0:100)),as.character(0:100))

})


test_that("equiv",{
  .dtDelayPkKnFx2=function(df){
    df%>%
      filter(joint=="Knee"&plane=="sag")%>%
      unistat(`60`:`100`,.f=function(x)which.max(x),.dir=">",.c=75-60+1)%>%
      # custom(`60`:`100`,cond="t[which.max(angle)]>75")%>%
      finish(featurename = "Delayed Peak Knee Flexion",fname = "DlyPkKFx")
  }

  suppressMessages(expect_equal(max(.dtDelayPkKnFx2(kinematics)-.dtDelayPkKnFx(kinematics)),0))

  .dtDblBump2=function(df){
    df%>%
      filter(joint=="Pel"&plane=="sag")%>%
      unistat(`0`:`100`,.f=.ROM,.dir=">",k=2)%>%
      unistat(`0`:`100`,.f=.period,.dir="==",.c=2)%>%
      unistat(`0`:`100`,.f=function(x)max(apply(t(.tc()$dblbump),2,cor,x)),.dir=">=",.c=0.8)%>%
      # corr(`0`:`100`,TC=.tc()$dblbump)%>%
      finish(featurename = "Increased ROM (double bump)",fname = "DblB")
  }
  suppressMessages(expect_equal(max(.dtDblBump2(kinematics)-.dtDblBump(kinematics)),0))
})
