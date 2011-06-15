
class read_only (fs: #ODBFSDisk.read_only) rwlock =
object 
  inherit ODBVFSLock.read_only fs rwlock 

  method real_filename fn = 
    fs#real_filename fn
end

class read_write (fs: #ODBFSDisk.read_write) rwlock =
object
  inherit ODBVFSLock.read_write fs rwlock

  method real_filename fn =
    fs#real_filename fn
end
