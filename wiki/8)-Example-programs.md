#Example programs

##Prime checking

[<pre>#p</pre>](http://brachylog.tryitonline.net/#code=I3A&input=MTM)

[<pre>$pl1</pre>](http://brachylog.tryitonline.net/#code=JHBsMQ&input=MTM)

[<pre>2;1\<I\<?'(:I=%0)</pre>](http://brachylog.tryitonline.net/#code=MjsxPEk8PycoOkk9JTAp&input=MTM)

###Explanation

<pre>#p                      <i>Input is a prime number<i></pre>


<pre>$p                      <i>The prime factorization of the Input…</i>
  l1                    <i>…has length 1</i></pre>

<pre>2                       <i>Input = 2</i>
  ;                     <i>Or</i>
   1&lt;I&lt;?                <i>1 &lt; I &lt; Input</i>
        '(     )        <i>The clause in parentheses is not provable:</i>  
          :I=             <i>Assign a value to I that fits its constraints</i>
             %0           <i>Input mod I = 0</i></pre>


##`"Hello, World!"`

[<pre>@Hw</pre>](http://brachylog.tryitonline.net/#code=QEh3&input=)

[<pre>"Hello, World!"w</pre>](http://brachylog.tryitonline.net/#code=IkhlbGxvLCBXb3JsZCEidw&input=)


###Explanation

`w` is the built-in predicate `Write` which prints its Input to `STDOUT`. `@H` and `"Hello, World!"` are both the string `Hello, World!`.

##Quine

[<pre>@Qw</pre>](http://brachylog.tryitonline.net/#code=QFF3&input=)

[<pre>"~c~s~cS:[34:S:34]rw"S:[34:S:34]rw</pre>](http://brachylog.tryitonline.net/#code=In5jfnN-Y1M6WzM0OlM6MzRdcnciUzpbMzQ6UzozNF1ydw&input=)

##Palindromization

[<pre>:Lc.r</pre>](http://brachylog.tryitonline.net/#code=OkxjLnI&input=IlRlc3Qi&args=Wg)

###Explanation

<pre>:Lc.       Output is the result of concatenating the Input and a variable L
   .r      Reversing the Output results in the Output</pre>

##Factor couples

[<pre>:1f
,A:B.>=*?,.=</pre>](http://brachylog.tryitonline.net/#code=OjFmCixBOkIuPj0qPywuPQ&input=MTI&args=Wg)

###Explanation

<pre>:1f                Find all valid outputs of predicate 1 (see below)

                   Predicate 1:
,A:B.                Output = [A, B]
     >=              A >= B
       *?            A * B = Input
         ,.=         Assign integer values to A and B which match those constraints</pre>