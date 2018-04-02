function list = read_list(listname)

% open file for reading
fid = fopen(listname,'r');

% read everything in file
readline = fgetl(fid);
count=1;
while readline ~= -1
    list{count} =  readline;
    count = count + 1;
    readline = fgetl(fid);
end

% close files
fclose(fid);