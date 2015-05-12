function res = cust_ceil(x)

if x > 1;
    res = 1;
elseif x < 0;
    res = 0;
else;
    res = x;
end

end