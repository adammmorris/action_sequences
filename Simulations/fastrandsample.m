%% fastrandsample
% A faster version of MATLAB's "randsample" function.
% Courtesy of Sam Gershman, June 2015. Original documentation included
% below.

function y = fastrandsample(p,n) 
      
     % Fast multinomially-distributed random numbers. 
     % 
     % USAGE: y = fastrandsample(p,[n]) 
     % 
     % INPUTS: 
     %   p - [1 x K] probability distribution 
     %   n - number of samples
     % 
     % OUTPUTS: 
     %   y - [1 x N] random numbers 
     % 
     % Sam Gershman, June 2015 
      
     [~, y] = histc(rand(1,n),[0 cumsum(p)]); 