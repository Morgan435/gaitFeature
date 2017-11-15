## assuming you have established connection 'con' to the database
tbl(con,"w_curve")%>%
  filter(app_id==XXXX)%>%
  inner_join(tbl(con,"w_structure"),by=c("structure_id"="id"))%>%
  collect%>%
  separate(points,as.character(0:100),sep=",",convert=T)%>%
  filter(grepl("Angles",abbr))%>%
  mutate(joint=gsub("New|Angles","",abbr),abbr=NULL,
         plane=c("sag","cor","tra")[as.numeric(plane)])%>%
  inner_join(tbl(con,"w_observation")%>%
               filter(app_id==XXXXX,condition_id==1)%>%
               inner_join(tbl(con,"w_variable"),by=c("var_id"="id"))%>%
               select(-var_id)%>%
               collect%>%
               spread(var_name,var_value,convert=T)%>%
               mutate(ofs=(`Opposite Foot Strike Frame`-`Stride Start Frame`)/(`Stride End Frame`-`Stride Start Frame`))%>%
               mutate(ofs=round(ofs*100))%>%
               select(condition_id,stride_nr,side,ofs))%>%
  mutate(curve_id=1:nrow(.))%>%
  filter(!is.na(joint))%>%
  select(curve_id,joint,side,plane,ofs,`0`:`100`)
