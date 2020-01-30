#' Encrypts a column (or columns) from a file, with a given password, generates
#' lookup tables, and zips the tables into an encrypted zip file with the
#' password. Files are (optionally) saved to a timestamped directory, and a
#' timestamped zip file is then (optionally) generated. The data with encrypted ID columns will be saved to the specified path, with the same file name as the original, but 
#'
#' It's your responsibility to securely erase the identifiable data. Use
#' something that specifically overwrites it, your operating system will not do
#' that by default.
#'
#' @param input.file.path Character file path readable by data.table::fread
#' @param col.names Character vector of columns to encrypt.
#' @param password The password with which to encrypt (Default: 'password')
#' @param save.encrypted.table Save encrypted data as CSV to table.save.path
#'   (Default: T)
#' @param save.lookup.table Save lookup tables for each encrypted column
#'   (Default: T)
#' @param save.password Save the password in a plain text file (Default: T)
#' @param save.zip Save all generated files in an encrypted ZIP file with same
#'   password, this probably requires Rtools, and for the zip function to be in
#'   Windows PATH (Default: T)
#' @param table.save.dir Character directory to save table with encrypted data
#'   to (Default: same as input)
#' @param lookup.save.dir Character directory to save lookup tables to (Default:
#'   '.' i.e. working directory)
#' @param encrypted.column.suffix Character to append to original column names
#'   to indicate that they are encrypted. (Default: '.IDencrypted')
#' @param encrypted.file.suffix Character to append to original file names
#'   to indicate that they are encrypted. (Default: '_IDencrypted')
#' @param output.to.console Say what's going on in the console (Default: T)
#'
#' @export
encrypt.col = function(input.file.path, 
                       col.names, 
                       password = 'password',
                       save.encrypted.table = T,
                       save.lookup.table = T,
                       save.password = T,
                       save.zip = T,
                       table.save.dir = NULL,
                       lookup.save.dir = '.',
                       encrypted.column.suffix = '.IDencrypted',
                       encrypted.file.suffix = '_IDencrypted',
                       output.to.console = T) {
  
  time.stamp = format(Sys.time(), "%Y-%m-%d-%H%M%S")
  
  lookup.save.dir.files = file.path(lookup.save.dir, time.stamp)
  dir.create(lookup.save.dir.files)
  
  #Generate output file name
  input.dir = dirname(input.file.path)
  input.file.name = basename(input.file.path)
  input.file.name.sans.ext = tools::file_path_sans_ext(input.file.name)
  #Set output of encrypted data to same as input if another directory is not provided.
  if (is.null(table.save.dir)) {
    table.save.dir = dirname(input.file.path)
  }
  table.save.path = file.path(
    table.save.dir,
    paste0(
      input.file.name.sans.ext,
      encrypted.file.suffix,
      '_',
      time.stamp,
      '.',
      tools::file_ext(input.file.name)
    )
  )
  
  save.zip.files = c()
  
  #Read in data
  input.dt = fread(input.file.path)
  
  for (col.name in col.names) {
    #Check if string and get column.
    if (!is.character(input.dt[,get(col.name)])) {
      stop(paste('Column', new.col.name, 'was detected as not being a character.'))
    }
    lookup.dt = unique(input.dt[,col.name,with=F])
    #Make new column name, check if it exists
    new.col.name = paste0(col.name, encrypted.column.suffix)
    if (new.col.name %in% names(input.dt)) {
      stop(paste('Column', new.col.name, encrypted.column.suffix))
    }
    
    
    #Create column for encrypted data
    data.table::set(
      lookup.dt,
      j = paste0(col.name, encrypted.column.suffix),
      value = NA_character_)
    
    #Helper function that encrypts a single row.
    encrypt.row.ind = function(row.ind) {
      data.table::set(lookup.dt,
                      i = row.ind,
                      j = paste0(col.name, encrypted.column.suffix),
                      value = safer::encrypt_string(lookup.dt[row.ind,get(col.name)],
                                                    key = password))
    }
    
    if (output.to.console) {
      message(paste('Encrypting',
                    col.name,
                    'to',
                    new.col.name,
                    '...'))
    }
    #Encrypt column in lookup table.
    lapply(X = 1:nrow(lookup.dt),
           FUN = encrypt.row.ind)
    #Encrypt in input
    new.col.vals = data.table:::merge.data.table(x = input.dt,
                                                  y = lookup.dt,
                                                  by = col.name)[,new.col.name, with = F]
    data.table::set(x = input.dt,
                    j = col.name,
                    value = new.col.vals)
    data.table::setnames(x = input.dt,
                         old = col.name,
                         new = new.col.name)
    
    if (output.to.console) {
      message('Done.')
    }
    
    #Save lookup table if requested.
    if (save.lookup.table) {
      lookup.dt.path = file.path(lookup.save.dir.files,
                                 paste0(input.file.name.sans.ext,
                                        '_',
                                        col.name,
                                        '.lookup.table.',
                                        time.stamp,
                                        '.csv'))
      
      if (output.to.console) {
        message(paste('Saving',
                      col.name,
                      'to',
                      lookup.dt.path))
      }
      data.table::fwrite(x = lookup.dt, 
                         file = lookup.dt.path)
      message('This file should be SECURELY erased using a utility like eraser.de')
      
      
      #Add to vector to be zipped if requested.
      if (save.zip) {
        save.zip.files = c(save.zip.files, lookup.dt.path)
      }
    }
    
  }
  
  #Save lookup table if requested.
  if (save.password) {
    password.path = file.path(lookup.save.dir.files,
                              paste0(input.file.name.sans.ext,
                                     '_',
                                     'password.',
                                     time.stamp,
                                     '.txt'))
    
    if (output.to.console) {
      message(paste('Saving',
                    'to',
                    password.path))
    }
    
    write(x = password,
          file = password.path)
    message('This file should be SECURELY erased using a utility like eraser.de')
    
    #Add to vector to be zipped if requested.
    if (save.zip) {
      save.zip.files = c(save.zip.files, password.path)
    }
  }
  
  #Save zip file
  if (length(save.zip.files) > 0) {
    zip.path = file.path(lookup.save.dir, 
                         paste0(input.file.name.sans.ext,
                                '_',
                                'id.data.', 
                                time.stamp, '.zip'))
    message(paste('Saving data to encrypted ZIP file at',
                  zip.path))
    
    utils::zip(zip.path,
               files = save.zip.files,
               flags = paste("--password", password))
  }
  
  #Save encrypted data if requested, and path provided.
  if (save.encrypted.table == T) {
    if (!is.null(table.save.path)) {
      
      if (output.to.console) {
        message(paste('Saving data with encrypted identifiable data',
                      'to',
                      table.save.path))
      }
      
      data.table::fwrite(x = input.dt,
                         file = table.save.path)
      
    } else {
      warning('Requested saving of output, but no path provided.')
    }
  }
  
}