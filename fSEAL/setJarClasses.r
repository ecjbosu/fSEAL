


setJarClasses <-function (jardir='/opt/jars/'){
require(rJava);

.jinit()
#load jars

#jar directory
#put in a system cache database
jarfiles = list.files(jardir, all.files = F, full.names = T, include.dirs=F);


ret=lapply(jarfiles, function(x) .jaddClassPath(x));
ret = .jclassPath();

#detach("package:rJava");

return(ret);

}                                             