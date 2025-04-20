rotation_from_rest <- function(rest_vec){
  gravity <- sign(rest_vec[2])*norm(rest_vec, type = "2")
  goal_vec <- c(0, gravity, 0)
  axis <- pracma::cross(rest_vec, goal_vec)
  angle <- acos(rest_vec[2]/gravity)
  
  unit_axis <- axis/norm(axis, type = "2")
  u1 <- unit_axis[1]
  u2 <- unit_axis[2]
  u3 <- unit_axis[3]
  
  rot_mat <- matrix(0, nrow = 3, ncol = 3)
  rot_mat[1,1] = u1^2* (1 - cos(angle))+ cos(angle)
  rot_mat[2,1] = u1*u2*(1 - cos(angle)) + u3*sin(angle)
  rot_mat[3,1] = u1*u3*(1 - cos(angle)) - u2*sin(angle)
  rot_mat[1,2] = u1*u2*(1 - cos(angle)) - u3 * sin(angle)
  rot_mat[2,2] = u2^2* (1 - cos(angle)) + cos(angle)
  rot_mat[3,2] = u2*u3*(1 - cos(angle)) + u1*sin(angle)
  rot_mat[1,3] = u1*u3*(1 - cos(angle)) + u2*sin(angle)
  rot_mat[2,3] = u2*u3*(1 - cos(angle)) - u1*sin(angle)
  rot_mat[3,3] = u3^2* (1 - cos(angle)) + cos(angle)
  
  print(rot_mat %*% rest_vec)
  
  return(list(rot_mat, gravity))
}
