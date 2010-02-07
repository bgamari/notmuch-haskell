OBJS = Notmuch.o Notmuch.hi NOTMUCH_H_hsc.o
LIBS = -lgmime-2.4 -lz -lnsl -lgobject-2.0 -lglib-2.0 -ltalloc -lxapian
NOTMUCHLIB = /local/src/notmuch/lib/notmuch.a

notmuchtest: $(OBJS)
	ghc --make -o notmuchtest NotmuchTest.hs $(NOTMUCHLIB) $(LIBS)

Notmuch.o Notmuch.hi: Notmuch.hs
	ghc --make Notmuch.hs
