


  let  getSuffixFrom s n =
    let len = String.length s in 
    if n < 0 then "" else
      let rec aux  stmp i =  
        if len <= i then
          stmp
        else
          aux  (stmp^(String.make 1 (s.[i]))) (i+1)
        
      in aux  "" n
      

  let makeSuffixList s = 
    let len = String.length s in 
    let rec aux stmp i =  
      if len <= i then
        stmp
      else
        aux  ((getSuffixFrom s i)::stmp) (i+1)
        
    in aux  [] 0
    
