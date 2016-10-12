import cbor
import json

class AnnotationsFile(object):
    def __init__(self, fname):
        """
        Read annotations from a file.

        Arguments:
          fname      The name of the CBOR file. A table-of-contents file is
                     also expected to be present.
        """
        self.cbor = open(fname, 'rb')
        self.toc  = json.load(open(fname+'.json'))

    def keys(self):
        """ The page names contained in an annotations file. """
        return self.toc.keys()

    def get(self, page):
        """ Lookup a page by name. Returns a Page or None """
        offset = self.toc.get(page)
        if offset is not None:
            self.cbor.seek(offset)
            return Page.from_cbor(cbor.load(self.cbor))
        return None

class Page(object):
    """
    The name and skeleton of a Wikipedia page.

    Attributes:
      page_name    The name of the page (str)
      skeleton     Its structure (a list of PageSkeletons)
    """
    def __init__(self, page_name, skeleton):
        self.page_name = page_name
        self.skeleton = skeleton

    @staticmethod
    def from_cbor(cbor):
        return Page(cbor[1], map(PageSkeleton.from_cbor, cbor[2]))

    def __str__(self):
        return "Page(%s)" % self.page_name

class PageSkeleton(object):
    """ A minimal representation of the structure of a Wikipedia page. """
    @staticmethod
    def from_cbor(cbor):
        tag = cbor[0]
        if tag == 0:
            return Section(cbor[1], map(PageSkeleton.from_cbor, cbor[2]))
        elif tag == 1:
            return Para(map(ParaBody.from_cbor, cbor[1]))

class Section(PageSkeleton):
    """
    A section of a Wikipedia page.

    Attributes:
      title       The title of a page (str)
      children    The PageSkeleton elements contained by the section
    """
    def __init__(self, title, children):
        self.title = title
        self.children = children

    def __str__(self, level=1):
        bar = "".join("=" for i in range(level))
        children = "".join(c.__str__(level=level+1) for c in self.children)
        return "%s %s %s\n\n%s" % (bar, self.title, bar, children)

class Para(PageSkeleton):
    """
    A paragraph within a Wikipedia page.

    Attributes:
      para_bodies    The content of the paragraph (a list of ParaBodys)
    """
    def __init__(self, para_bodies):
        self.para_bodies = para_bodies

    def __str__(self, level=None):
        return "".join(b.__str__() for b in self.para_bodies)

class ParaBody(object):
    """
    A bit of content of a paragraph (either plain text or a link)
    """
    @staticmethod
    def from_cbor(cbor):
        tag = cbor[0]
        if tag == 0:
            return ParaText(cbor[1])
        elif tag == 1:
            return ParaLink(cbor[1][1], cbor[2])

class ParaText(ParaBody):
    """
    A bit of plain text from a paragraph.

    Attributes:
      text      The text
    """
    def __init__(self, text):
        self.text = text

    def __str__(self, level=None):
        return self.text

class ParaLink(ParaBody):
    """
    A link within a paragraph.

    Attributes:
      page          The page name of the link target
      anchor_text   The anchor text of the link
    """
    def __init__(self, page, anchor_text):
        self.page = page
        self.anchor_text = anchor_text

    def __str__(self, level=None):
        return "[%s](%s)" % (self.anchor_text, self.page)
