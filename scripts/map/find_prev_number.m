function num = find_prev_number(str,expression)
strsplit = regexp(str,expression,'split');
strsplit = strsplit{1};
num= str2num(strsplit(end));
if(isempty(num))
    num=0;
end
end