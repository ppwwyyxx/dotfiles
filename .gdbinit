python
import sys
sys.path.insert(0, '/home/wyx/.gdb/python')
from libstdcxx.v6.printers import register_libstdcxx_printers
register_libstdcxx_printers (None)
end
