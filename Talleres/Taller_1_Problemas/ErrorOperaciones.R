#m_V representa la velocidad medida
#m_eV representa el error de la velocidad
#m_T representa el tiempo medido
#m_eT representa el error del tiempo
calcErrorD = function(m_V, m_T, m_eV, m_eT)
{
  v=m_V
  t=m_T
  d=v*t
  absError = v*m_eV+t*m_eT
  relError =  m_eV/v + m_eT/t
  cat("Distancia: ",d," Error Absoluto: " ,absError, "Error Relativo: ", relError )
}
calcErrorD(4,5,0.1,0.1)