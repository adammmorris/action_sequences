function [subjMarkers] = getSubjMarkers(id)
% This array is going to be populated with all the points at which a new
%   subject begins
subjMarkers = ones(1);
curSubject = 1;

% Parse input
% Are we dealing with #s?
if isnumeric(id)
    for thisDataPt = 2:length(id)
        if id(thisDataPt) ~= id(thisDataPt - 1)
            curSubject = curSubject + 1;
            subjMarkers(curSubject) = thisDataPt;
        end
    end
% Are we dealing with strings?
else
    for thisDataPt = 2:length(id)
        if strcmpi(id(thisDataPt), id(thisDataPt - 1)) == 0
            % We've hit a new person
            curSubject = curSubject + 1;
            subjMarkers(curSubject) = thisDataPt;
        end
    end
end

end