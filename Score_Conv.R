
Score_Conv = function(score) {
  if(score >= 90) {
    res = "优"
  } else if(score >= 80) {
    res = "良"
  } else if(score >= 70) {
    res = "中"
  } else if(score >= 60) { res = "及格"
  } else {
    res = "不及格" }
  res }