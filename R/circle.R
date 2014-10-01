circle <- function(
  x,
  y,
  r,
  locnum=100,
  ...)
{
polygon(x+r*cos(seq(0,2*pi,len=locnum)), y+r*sin(seq(0,2*pi,len=locnum)), ...)
}
