main=interact g
isP x=x/=' '&&(x<'a'||x>'z')
g(' ':p:x)|isP p||p==' '=g(p:x)
g(p:x:y)|isP p&&x/=' '=p:g(' ':x:y)
g(x:y)=x:g y
g[]=[]
