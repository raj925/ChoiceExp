% x is the array of values to derive a dist from
% y is the size of groupings
function f = freqDis(x,y)
    xMin = min(x);
    xMax = max(x);
    yMin = floor(xMin/y)*y;
    yMax = ceil(xMax/y)*y;
    groupings = [];
    count = yMin;
    while count <= yMax
        groupings = [groupings, count];
        count = count + y;
    end
    freq = histc(x,groupings);
    %relativefreq = ncount/length(x);
    bar(groupings+y/2, freq, 1);
    f = freq;
end