program radixsort(input, output);
const numelts = 100;
    abovelts = 101;
type arraytype = array[1..numelts] of integer;
    aptr = 1..numelts;
    aptr2 = 0..abovelts;
var x: arraytype;
    i,n: integer;
procedure radix(var a: arraytype; n: aptr);
const m=4; {number of digits}
type nodetype = record
    info: integer;
    next: aptr2;
    end;
var node: array[1..numelts] of nodetype;
    front: array[0..10] of aptr2;
    rear: array[0..9] of aptr2;
    p: aptr;
    first,q,i,j: aptr2;
    y,expon,k: integer;
begin
    {initialize linked list}
    for i:= 1 to n-1 do
        begin
        node[i].info := x[i];
        node[i].next := i+1
    end;
    node[n].info := x[n];
    node[n].next := 0;
    first := 1; {first is the head of the linked list}
    for k:= 1 to m do
    {m is the number of digits in the numbers}
        begin
        for i:= 0 to 9 do rear[i] := 0;
        for i:= 0 to 10 do front[i] := 0; {initializa queues}
        {process each element on the list}
        while first <> 0 do
            begin
            p := first;
            first := node[first].next;
            y := node[p].info;
            {extract kth digit}
            expon := 1;
            for i:= 1 to k-1 do expon := expon * 10;
            j := (y div expon) mod 10;
            {insert y into queue[j]}
            q := rear[j];
            if q = 0
                then front[j] := p
                else node[q].next := p;
                rear[j] := p;
            end;
            {At this point each record is in its proper queue based on}
            {digit k. Now a single list must be formed from all the queue}
            {elements. Find the first elements.}
            j := 0;
            while (j <= 9) and (front[j] = 0) do j:=j+1;
            first := front[j];
            {link up remaining queues}
            while j <= 9 do {check if finished}
                begin {find next element}
                i := j + 1;
                while (i<=9) and (front[i]=0) do i:=i+1;
                if i <= 9 then
                    begin
                    p := i;
                    node[rear[j]].next := front[i];
                end;
                j := i
            end;
            node[rear[p]].next := 0
        end;
        {copy back to original array}
        for i:=1 to n do
            begin
            x[i] := node[first].info;
            first := node[first].next
        end
    end;
        begin
        n:=50;
        i:=0;
        randomize;
        repeat i:=i+1;
        x[i] := random(1000)+1;
        until i=n;
        for i:=1 to n do
        begin write(x[i]); write(' ') end;
    writeln();
    radix(x,n);
    for i:=1 to n do
    begin write(x[i]); write(' ') end;
end.