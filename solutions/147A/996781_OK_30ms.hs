main=interact g
isP x=x/=' '&&(x<'a'||x>'z')
g(' ':p:x)|isP p=g(p:x)
g(' ':' ':x)=g(' ':x)
g(p:x:y)|isP p&&x/=' '=p:g(' ':x:y)
g(x:y)=x:g y
g[]=[]
