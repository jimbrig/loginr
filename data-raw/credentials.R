
credentials <- data.frame(
  username = c("admin", "user1"),
  password_hash = c(
    digest::digest("Admin123!", algo = "sha256"),
    digest::digest("User123!", algo = "sha256")
  )
)

usethis::use_data(credentials, overwrite = TRUE)
