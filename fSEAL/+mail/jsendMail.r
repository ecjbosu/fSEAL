#===============================================================================
# Name   : jsendMail
# Author : Joe W. Byers
# Date   : 10/15/2013
# Version: 1.0001
# Aim    : java sendmail for R
# Mail   : <<<financialseal@financialseal.com>>>
#===============================================================================
#

jsendMail <- function(to, from, subject, msgBody, smtpServer,
  mimemessagetype = 'html',
  pathToJars = '/usr/share/java', charset = 'UTF-8') {

#require(rJava)


#setJarClasses(pathToJars);

#required for setTest calls for message
#charset = 'UTF-8';

#===============================================================================
#Java Import attechments
attach(javaImport(packages = "javax.mail"), pos= 2, name = 'javax.mail');
attach(javaImport(packages = "javax.mail.internet"), pos=2, name= 'javax.mail.internet');
attach(javaImport(packages = "javax.activation"), pos=2, name= 'javax.activation');
#attach( javaImport(packages = "java.io"), pos= 2, name = 'java.io');
#attach(javaImport(packages = "java.net"), pos=2, name= 'java.net');
#attach(javaImport(packages = "java.util"), pos=2, name= 'java.util');
#attach(javaImport(packages = "java.lang"), pos=2, name= 'java.lang');
##===============================================================================


props = .jnew('java/lang/System');
props  = props$getProperties()$clone();

props$put('mail.smtp.host',smtpServer);


session = Session$getDefaultInstance(props);

msg     = .jnew(MimeMessage, session);

msg$setFrom( .jnew(InternetAddress,from));

ret = NULL;

ret <- sapply(to, function(x) msg$addRecipient(Message$RecipientType$TO,
  .jnew(InternetAddress, x)));
msg$setSubject(subject);

if (mimemessagetype == 'html')
     ret <- try(msg$setText(msgBody, charset, mimemessagetype));
if (mimemessagetype == 'text')
     ret <- try(msg$setText(msgBody, charset));

#send the message
if (is.null(ret))
  ret <- try(Transport$send(msg));

#===============================================================================
#clear up
rm(list = c('msg', 'session', 'props'))
detach(name = 'javax.mail');
detach(name= 'javax.mail.internet');
detach(name= 'javax.activation');

return(ret)
}