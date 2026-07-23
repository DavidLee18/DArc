/* macOS has no <malloc.h>; the allocator declarations SREP uses live in
 * <stdlib.h> there. Only on the Darwin include path (see srep/compile). */
#pragma once
#include <stdlib.h>
