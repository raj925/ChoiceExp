function err = stdErr(val)
    val(isnan(val))=0;
    err = std(val)/sqrt(length(val));
    return;
end