# Next-Generation AI-Assisted Workflow System

## Overview

This document outlines the comprehensive workload for implementing an advanced AI-powered automation system that extends beyond typical coding agents. The system integrates research automation, persistent memory, browser control, and university/work task automation.

---

## Feature Summary

| Feature | Description | Priority |
|---------|-------------|----------|
| **Browser1 (Research Crawler)** | arXiv/Semantic Scholar検索, GitHub解析, Multi-page crawl | HIGH |
| **Browser2 (Private Chrome)** | Chrome automation for authenticated sessions | HIGH |
| **GPT 5.2 Pro Consulting** | Human-in-the-loop GPT consultation automation | HIGH |
| **Deep Research Mode** | Multi-step research with report generation | HIGH |
| **Memory System** | Persistent knowledge storage in `.hikettei/memory/` | HIGH |
| **Memory Panel** | UI to display memory/TODO files | MEDIUM |
| **PDF Pipeline** | PDF viewing + Screenshot + OCR integration | MEDIUM |
| **MCP Tool Extension** | Enhanced MCP tools for all features | HIGH |

---

## Research Findings

### 1. Browser1 - Research Crawler (Public Web)

#### Approach: 専門的なリサーチ用クローラー

Claude自体がWebSearch Toolを持つため、Browser1は**Deep Research専用**の高度なクロール機能に特化：

**Browser1 の役割 (Claude WebSearchとの差別化):**

| 機能 | Claude WebSearch | Browser1 |
|------|-----------------|----------|
| 単発検索 | ✅ 得意 | ❌ 不要 |
| 複数ページ巡回 | ❌ | ✅ リンクを辿って調査 |
| 学術論文検索 | △ 限定的 | ✅ arXiv/Semantic Scholar API |
| GitHub調査 | △ 限定的 | ✅ リポジトリ構造解析 |
| ページ内リンク抽出 | ❌ | ✅ 関連ページ発見 |
| コンテンツ保存 | ❌ | ✅ Memory連携 |

**Architecture:**
```
┌─────────────────────────────────────────────────────────────┐
│                   Deep Research Agent                        │
│  ┌──────────────────────────────────────────────────────┐   │
│  │  1. Claude WebSearch で初期調査                        │   │
│  │  2. Browser1 で深掘り (リンク巡回、論文検索)            │   │
│  │  3. 結果を Memory に保存                               │   │
│  └──────────────────────────────────────────────────────┘   │
└─────────────────────────────────────────────────────────────┘
```

**Core Features:**

**1. Academic Paper Search (arXiv/Semantic Scholar)**
```python
import arxiv

def search_papers(query: str, max_results: int = 10):
    """Search arXiv for academic papers."""
    search = arxiv.Search(
        query=query,
        max_results=max_results,
        sort_by=arxiv.SortCriterion.Relevance
    )
    return [
        {"title": r.title, "url": r.pdf_url, "abstract": r.summary}
        for r in search.results()
    ]
```

**2. GitHub Repository Analysis**
```python
def analyze_github_repo(repo_url: str):
    """Analyze GitHub repository structure."""
    # Clone to .hikettei/memory/cloned_repos/
    # Extract: README, file structure, key code patterns
    pass

def search_github_code(query: str, language: str = None):
    """Search GitHub for code examples."""
    # Use GitHub Search API
    pass
```

**3. Multi-Page Crawl**
```python
def research_crawl(start_url: str, depth: int = 2):
    """Crawl starting URL and follow relevant links."""
    visited = set()
    results = []
    # BFS crawl with relevance filtering
    return results
```

**MCP Tools:**
```json
{
  "tools": [
    {"name": "arxiv_search", "description": "Search arXiv papers by query"},
    {"name": "arxiv_download", "description": "Download paper PDF to memory"},
    {"name": "github_analyze", "description": "Analyze GitHub repo structure"},
    {"name": "github_clone", "description": "Clone repo for local investigation"},
    {"name": "research_crawl", "description": "Crawl and extract from multiple pages"}
  ]
}
```

**References:**
- [arxiv.py](https://github.com/lukasschwab/arxiv.py) - arXiv API wrapper
- [Semantic Scholar API](https://www.semanticscholar.org/product/api) - 225M+ papers
---

### 2. Chrome Automation (Browser2)

#### Recommended: Playwright + Chrome DevTools Protocol

**Architecture:**
```
┌─────────────────────────────────────────────────────────────┐
│                         Emacs                                │
│  ┌──────────────┐    ┌──────────────┐    ┌──────────────┐   │
│  │ MCP Server   │    │ Browser      │    │ Permission   │   │
│  │ (HTTP)       │◄──►│ Controller   │◄──►│ Manager      │   │
│  └──────────────┘    └──────────────┘    └──────────────┘   │
└─────────────────────────────────────────────────────────────┘
                              │
                              ▼ WebSocket (CDP)
┌─────────────────────────────────────────────────────────────┐
│  Chrome (--remote-debugging-port=9222)                       │
│  ┌──────────────┐    ┌──────────────┐    ┌──────────────┐   │
│  │ GitHub       │    │ ChatGPT      │    │ keio.jp      │   │
│  │ (logged in)  │    │ (logged in)  │    │ (logged in)  │   │
│  └──────────────┘    └──────────────┘    └──────────────┘   │
└─────────────────────────────────────────────────────────────┘
```

**Key Libraries:**
- [Playwright Python](https://github.com/microsoft/playwright-python) - Best balance of features
- [pychrome](https://github.com/nicoleputzolu/pychrome) - Direct CDP access
- [Playwright MCP](https://github.com/microsoft/playwright-mcp) - Official MCP server

**Connecting to User's Chrome:**
```python
from playwright.sync_api import sync_playwright

with sync_playwright() as p:
    # Connect to Chrome started with --remote-debugging-port=9222
    browser = p.chromium.connect_over_cdp("http://localhost:9222")
    context = browser.contexts[0]  # User's logged-in context
    page = context.pages[0]
```

**MCP-Based Browser Tools:**
- [Chrome DevTools MCP](https://developer.chrome.com/blog/chrome-devtools-mcp) - Google's official MCP server
- [Browser Use MCP](https://docs.browser-use.com/customize/integrations/mcp-server) - AI-powered browser automation
- [Browser MCP](https://github.com/BrowserMCP/mcp) - Chrome extension + MCP server

**Security Considerations:**
- Debug port exposes full browser control to localhost
- Always require explicit user permission
- Never commit profile paths or cookies

**References:**
- [Playwright MCP Guide](https://medium.com/@bluudit/playwright-mcp-comprehensive-guide-to-ai-powered-browser-automation-in-2025-712c9fd6cffa)
- [Browser Automation MCP Servers Guide](https://www.skyvern.com/blog/browser-automation-mcp-servers-guide/)

---

### 3. Deep Research Mode

#### Architecture: Multi-Agent Plan-and-Execute

**What is Deep Research?**
Deep research describes AI assistants that go beyond simple Q&A—planning sub-questions, searching broadly, cross-examining information, and synthesizing detailed reports with citations.

**Open Source Implementations:**

| Project | GitHub | Performance |
|---------|--------|-------------|
| **GPT Researcher** | [assafelovic/gpt-researcher](https://github.com/assafelovic/gpt-researcher) | Top performer in CMU DeepResearchGym |
| **LangChain Open Deep Research** | [langchain-ai/open_deep_research](https://github.com/langchain-ai/open_deep_research) | #6 on Deep Research Bench (0.4344) |
| **Tongyi DeepResearch** | [Alibaba-NLP/DeepResearch](https://github.com/Alibaba-NLP/DeepResearch) | 32.9 on HLE, 43.4 on BrowseComp |

**Workflow:**
```
1. Query Analysis
   └── Break down into sub-questions

2. Parallel Research
   ├── Web Search (EWW/w3m)
   ├── Academic Search (arXiv, Semantic Scholar)
   ├── GitHub Repository Search
   └── Local Knowledge Base

3. Information Synthesis
   ├── Cross-reference findings
   ├── Extract key insights
   └── Identify gaps → More research

4. Report Generation
   ├── Structured markdown report
   ├── Citations with sources
   └── Save to .hikettei/memory/
```

**Academic Paper APIs:**
- [arXiv API](https://info.arxiv.org/help/api/index.html) - 1M+ papers, free access
- [arxiv.py](https://github.com/lukasschwab/arxiv.py) - Python wrapper with download support
- [Semantic Scholar API](https://www.semanticscholar.org/product/api) - 225M+ papers, recommendations
- [MCP Paper Search Tools](https://github.com/openags/paper-search-mcp) - arXiv, PubMed, bioRxiv via MCP

**References:**
- [OpenAI Deep Research](https://openai.com/index/introducing-deep-research/)
- [LangChain Open Deep Research Blog](https://blog.langchain.com/open-deep-research/)
- [HuggingFace Open Deep Research](https://huggingface.co/blog/open-deep-research)

---

### 4. Persistent Memory System

#### Architecture: Hierarchical Memory with Graph Relationships

**Memory Types:**
- **Working Memory** - Current context within session
- **Short-term Memory** - Recent interactions, temporary notes
- **Long-term Memory** - Persistent facts, research findings, preferences

**Key Frameworks:**

| Framework | GitHub | Key Feature |
|-----------|--------|-------------|
| **Mem0** | [mem0ai/mem0](https://github.com/mem0ai/mem0) | 26% accuracy boost, graph memory (Mem0g) |
| **Cognee** | [topoteretes/cognee](https://github.com/topoteretes/cognee) | Knowledge graphs + vector search |
| **MCP Memory Service** | [doobidoo/mcp-memory-service](https://github.com/doobidoo/mcp-memory-service) | 5ms semantic search, persistent storage |

**Proposed Directory Structure:**
```
{workspace}/
└── .hikettei/
    ├── sessions.json           # Session history
    ├── TODO.md                 # Task list
    └── memory/
        ├── index.json          # Memory index with embeddings
        ├── research/
        │   ├── {topic}/
        │   │   ├── report.md   # Generated report
        │   │   ├── sources.json # Citation data
        │   │   └── papers/     # Downloaded PDFs
        │   └── ...
        ├── facts/
        │   └── {category}.json # Extracted facts
        ├── preferences/
        │   └── user.json       # User preferences
        └── cloned_repos/
            └── {repo}/         # Cloned reference repos
```

**Context Compaction Strategy:**
```
1. Extract key facts from conversation
2. Store in structured memory
3. On context limit:
   - Summarize recent history
   - Load relevant memories via semantic search
   - Inject as system context
```

**Performance (Mem0):**
- 26% accuracy improvement over full-context
- 91% lower p95 latency
- 90%+ token cost reduction

**References:**
- [Mem0 Research Paper](https://arxiv.org/abs/2504.19413)
- [Memory in AI Agents Survey](https://arxiv.org/abs/2512.13564)
- [Context Engineering Guide](https://mem0.ai/blog/context-engineering-ai-agents-guide)

---

### 5. PDF/OCR Pipeline

#### Tools:

| Component | Recommended | Alternative |
|-----------|-------------|-------------|
| PDF Viewing | pdf-tools | doc-view |
| Text Extraction | pdftotext (poppler) | pdfplumber |
| OCR | Tesseract | EasyOCR |
| Screenshots | screencapture (macOS) | - |
| LaTeX | AUCTeX + latexmk | - |

**Installation (macOS):**
```bash
brew install poppler tesseract tesseract-lang
pip install pdfplumber easyocr
```

**Pipeline: PDF → Screenshot → OCR → AI:**
```elisp
(defun pdf-ai-extract-page (pdf-file page)
  "Extract text from PAGE of PDF-FILE using best method."
  (if (pdf-has-text-layer-p pdf-file page)
      (pdf-extract-text pdf-file page)
    (pdf-ocr-page pdf-file page)))
```

**References:**
- [pdf-tools](https://github.com/vedang/pdf-tools)
- [Tesseract OCR](https://github.com/tesseract-ocr/tesseract)

---

### 6. keio.jp / University Portal Automation

**Challenge:** Shibboleth SSO authentication is complex multi-step flow

**Approach:**
1. User logs in manually to Chrome
2. Connect to running Chrome via CDP
3. Navigate using existing session cookies
4. Always require explicit permission

**Available Services via keio.jp SSO:**
- KOSMOS My Library - Book/journal search
- K-LMS (Canvas LMS) - Course management
- Remote Access - E-journals from off-campus
- Google Workspace - Keio Mail, Calendar

**References:**
- [keio.jp User's Manual](https://www.itc.keio.ac.jp/en/keiojp_manual.html)

---

### 7. GPT 5.2 Pro Consulting Automation

**Purpose:** 自動的にGPT 5.2 Proを開いて、実装についての相談を行う

**Use Cases:**
- 複雑な実装方針についてセカンドオピニオンを取得
- コードレビューの自動化
- エラー解決の相談
- アーキテクチャ設計のディスカッション

**Bot検出回避戦略: Human-in-the-Loop**

Bot検出を回避するため、**Enterキーのみ人間が押す**方式を採用：

```
┌─────────────────────────────────────────────────────────────┐
│  AI (Claude) が自動で行う部分:                               │
│  1. ChatGPTページを開く                                      │
│  2. GPT 5.2 Proモデルを選択                                  │
│  3. プロンプトをテキストエリアに入力                          │
│  4. Emacsに「準備完了」を通知                                 │
└─────────────────────────────────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────┐
│  人間が行う部分:                                             │
│  • Emacsの通知を見て、Chromeに切り替え                       │
│  • 内容を確認（必要なら編集）                                 │
│  • Enterキーを押して送信                                     │
└─────────────────────────────────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────┐
│  AI (Claude) が自動で行う部分:                               │
│  5. GPTの応答を待機・検出                                    │
│  6. 応答テキストを抽出                                       │
│  7. Memoryに保存 & Claudeに返却                              │
└─────────────────────────────────────────────────────────────┘
```

**Architecture:**
```
┌─────────────────────────────────────────────────────────────┐
│                      Emacs (Claude)                          │
│  ┌──────────────────────────────────────────────────────┐   │
│  │  gpt_consult MCP Tool                                 │   │
│  │  → Prepare prompt → Wait for human Enter              │   │
│  │  → Extract response → Return to Claude                │   │
│  └──────────────────────────────────────────────────────┘   │
└─────────────────────────────────────────────────────────────┘
                              │
                              ▼ Chrome CDP
┌─────────────────────────────────────────────────────────────┐
│  Chrome (chat.openai.com - GPT 5.2 Pro)                      │
│  ┌──────────────────────────────────────────────────────┐   │
│  │  [Prompt ready in textarea]                           │   │
│  │                                                        │   │
│  │  ⏳ Waiting for human to press Enter...               │   │
│  │                                                        │   │
│  │  Emacs notification: "GPT consultation ready.         │   │
│  │  Switch to Chrome and press Enter to send."           │   │
│  └──────────────────────────────────────────────────────┘   │
└─────────────────────────────────────────────────────────────┘
```

**Workflow:**
```python
class GPTConsultant:
    def consult(self, topic: str, context: str, question: str) -> str:
        """Consult GPT 5.2 Pro with human-in-the-loop."""
        
        # 1. Build structured prompt
        prompt = self.build_prompt(topic, context, question)
        
        # 2. Navigate to ChatGPT and select model
        self.chrome.navigate("https://chat.openai.com")
        self.chrome.select_model("gpt-5.2-pro")
        
        # 3. Type prompt into textarea (but DON'T send)
        self.chrome.type_message(prompt, send=False)
        
        # 4. Notify user and wait for response to appear
        self.emacs.notify(
            "GPT consultation ready",
            "Switch to Chrome, review the prompt, and press Enter to send."
        )
        
        # 5. Poll for response (detects when GPT starts responding)
        response = self.chrome.wait_for_response(timeout=300)
        
        # 6. Save to memory
        self.memory.store({
            "type": "gpt_consultation",
            "topic": topic,
            "response": response
        })
        
        return response
```

**MCP Tool Definition:**
```json
{
  "name": "gpt_consult",
  "description": "Prepare GPT 5.2 Pro consultation (human presses Enter)",
  "inputSchema": {
    "type": "object",
    "properties": {
      "topic": {"type": "string", "description": "Consultation topic"},
      "context": {"type": "string", "description": "Relevant code/context"},
      "question": {"type": "string", "description": "Question for GPT"}
    },
    "required": ["topic", "question"]
  }
}
```

**Emacs Notification UI:**
```
┌──────────────────────────────────────────────────────────┐
│  🤖 GPT Consultation Ready                                │
├──────────────────────────────────────────────────────────┤
│                                                          │
│  Topic: Memory system architecture                       │
│                                                          │
│  Prompt has been prepared in Chrome.                     │
│                                                          │
│  Next steps:                                             │
│  1. Switch to Chrome (Cmd+Tab)                           │
│  2. Review the prompt (edit if needed)                   │
│  3. Press Enter to send                                  │
│                                                          │
│  Claude will automatically detect the response.          │
│                                                          │
│  [Open Chrome]  [Cancel]                                 │
│                                                          │
└──────────────────────────────────────────────────────────┘
```

**Response Detection:**
```python
def wait_for_response(self, timeout=300):
    """Wait for GPT to finish responding."""
    start = time.time()
    
    while time.time() - start < timeout:
        # Check if response element exists and is complete
        response = self.chrome.evaluate("""
            const messages = document.querySelectorAll('[data-message-author-role="assistant"]');
            const last = messages[messages.length - 1];
            if (last && !document.querySelector('.result-streaming')) {
                return last.innerText;
            }
            return null;
        """)
        
        if response:
            return response
        
        time.sleep(1)
    
    raise TimeoutError("GPT response timeout")
```

**Integration with Deep Research:**
```
Research Workflow:
1. Web/arXiv research → Identify complex questions
2. gpt_consult → Prepare prompt, notify user
3. Human reviews & presses Enter
4. AI extracts response & synthesizes report
```

**Benefits of Human-in-the-Loop:**
- Bot検出を完全に回避
- 人間が内容を確認・編集できる
- OpenAI ToS違反のリスク軽減
- 誤送信防止

### Phase 1: Foundation (2-3 weeks)

#### 1.1 Memory System Core
**Files:** `hikettei/mcp/memory.el`, `hikettei/mcp/memory.py`

- [ ] Design `.hikettei/memory/` directory structure
- [ ] Implement memory index with JSON storage
- [ ] Add MCP tools: `memory_store`, `memory_search`, `memory_list`
- [ ] Implement semantic search with embeddings (sentence-transformers)
- [ ] Add memory compaction on context limit

**New MCP Tools:**
```json
{
  "tools": [
    {"name": "memory_store", "description": "Store information in persistent memory"},
    {"name": "memory_search", "description": "Semantic search across memories"},
    {"name": "memory_list", "description": "List memories by category"},
    {"name": "memory_delete", "description": "Remove specific memories"}
  ]
}
```

#### 1.2 Memory Panel UI
**Files:** `hikettei/panel/memory.el`

- [ ] Create new panel for memory visualization
- [ ] Display `.hikettei/memory/` hierarchy as tree
- [ ] Show `TODO.md` with interactive checkboxes
- [ ] Add research report viewer
- [ ] Integrate with existing multi-panel system

**Keybinding:** `C-x j y` → Memory panel

---

### Phase 2: Browser Integration (2-3 weeks)

#### 2.1 Browser1 - Research Crawler
**Files:** `hikettei/mcp/research-crawler.el`

- [ ] arXiv API integration (url-retrieve + XML parsing)
- [ ] Semantic Scholar API integration (JSON)
- [ ] GitHub repository analysis (magit integration)
- [ ] Multi-page crawl (url-retrieve-synchronously)
- [ ] Memory system連携

**MCP Tools:** `arxiv_search`, `arxiv_download`, `github_analyze`, `github_clone`, `research_crawl`
```

#### 2.2 Browser2 - Chrome Automation
**Files:** `hikettei/mcp/chrome-controller.py`, `hikettei/mcp/chrome.el`

- [ ] Implement Chrome CDP connection via Playwright
- [ ] Add permission system (always-ask for private browser)
- [ ] MCP tools: `chrome_navigate`, `chrome_click`, `chrome_type`, `chrome_screenshot`
- [ ] Support for GitHub, keio.jp, ChatGPT

**Permission Flow:**
```
1. AI requests chrome_* tool
2. Emacs shows permission dialog:
   "AI wants to navigate Chrome to [URL]. Allow? [y/n/always]"
3. User approves → Execute action
4. Log all actions in .hikettei/browser_log.json
```

**New MCP Tools:**
```json
{
  "tools": [
    {"name": "chrome_navigate", "description": "Navigate Chrome to URL (requires permission)"},
    {"name": "chrome_click", "description": "Click element by selector"},
    {"name": "chrome_type", "description": "Type text into element"},
    {"name": "chrome_get_content", "description": "Get page text content"},
    {"name": "chrome_screenshot", "description": "Take screenshot of page"}
  ]
}
}
```

#### 2.3 GPT 5.2 Pro Consulting
**Files:** `hikettei/mcp/gpt-consultant.py`, `hikettei/mcp/gpt-consultant.el`

- [ ] Implement Human-in-the-Loop workflow
- [ ] Auto-navigate to chat.openai.com
- [ ] Auto-select GPT 5.2 Pro model
- [ ] Type prompt without sending (wait for human Enter)
- [ ] Implement response detection and extraction
- [ ] Add Emacs notification UI ("Ready to send")
- [ ] Save consultations to memory

**New MCP Tool:**
```json
{
  "tools": [
    {
      "name": "gpt_consult",
      "description": "Prepare GPT 5.2 Pro consultation (human presses Enter to send)",
      "inputSchema": {
        "properties": {
          "topic": {"type": "string"},
          "context": {"type": "string"},
          "question": {"type": "string"}
        }
      }
    }
  ]
}
```

---

### Phase 3: Deep Research Mode (2-3 weeks)

#### 3.1 Research Agent
**Files:** `hikettei/mcp/deep-research.py`, `hikettei/panel/research.el`

- [ ] Implement multi-step research workflow
- [ ] Integrate arXiv API (arxiv.py)
- [ ] Integrate Semantic Scholar API
- [ ] Add web search via EWW
- [ ] Implement report generation

**Workflow Implementation:**
```python
class DeepResearchAgent:
    def research(self, query: str) -> Report:
        # 1. Plan sub-questions
        plan = self.plan_research(query)
        
        # 2. Execute parallel research
        results = []
        for subq in plan.sub_questions:
            results.extend(self.search_web(subq))
            results.extend(self.search_arxiv(subq))
            results.extend(self.search_local_memory(subq))
        
        # 3. Synthesize findings
        synthesis = self.synthesize(results)
        
        # 4. Generate report
        report = self.generate_report(query, synthesis)
        
        # 5. Save to memory
        self.save_to_memory(report)
        
        return report
```

#### 3.2 GitHub Repository Cloning
**Files:** `hikettei/mcp/git-tools.el`

- [ ] MCP tool: `git_clone_reference` - Clone repo for investigation
- [ ] Auto-organize in `.hikettei/memory/cloned_repos/`
- [ ] Implement cleanup of old clones

---

### Phase 4: PDF/Document Pipeline (1-2 weeks)

#### 4.1 PDF Integration
**Files:** `hikettei/mcp/pdf.el`, `hikettei/mcp/ocr.py`

- [ ] Integrate pdf-tools with MCP
- [ ] Implement page-level text extraction
- [ ] Add OCR fallback for scanned PDFs
- [ ] MCP tools: `pdf_open`, `pdf_extract_text`, `pdf_ocr_page`

#### 4.2 Screenshot + OCR
**Files:** `hikettei/mcp/screenshot.el` (extend)

- [ ] Extend existing screenshot tool
- [ ] Add OCR pipeline: screenshot → tesseract → text
- [ ] Support for PDF page screenshots

---

### Phase 5: Integration & Polish (1-2 weeks)

#### 5.1 Unified Workflow
- [ ] Connect all components via MCP
- [ ] Implement context-aware tool selection
- [ ] Add workflow templates (research, assignment, code review)

#### 5.2 UI Polish
- [ ] Update tab bar with new panels
- [ ] Add status indicators for background tasks
- [ ] Implement notification system for completed research

---

## File Structure (Proposed)

```
hikettei/
├── mcp/
│   ├── mcp-server.el      # Existing - extend with new tools
│   ├── file-editor.el     # Existing
│   ├── screenshot.el      # Existing - extend with OCR
│   ├── memory.el          # NEW: Memory system Elisp interface
│   ├── memory.py          # NEW: Memory backend with embeddings
│   ├── browser.el         # NEW: Text browser (EWW) MCP tools
│   ├── chrome.el          # NEW: Chrome automation Elisp interface
│   ├── chrome-controller.py # NEW: Playwright CDP controller
│   ├── deep-research.py   # NEW: Research agent
│   ├── pdf.el             # NEW: PDF tools
│   ├── ocr.py             # NEW: OCR backend
│   └── git-tools.el       # NEW: Git operations for research
├── panel/
│   ├── autopilot.el       # Existing
│   ├── explore.el         # Existing - enhance with EWW
│   ├── memory.el          # NEW: Memory visualization panel
│   └── research.el        # NEW: Research status panel
└── ...
```

---

## Dependencies

### Python Packages
```
playwright>=1.40.0
arxiv>=2.0.0
sentence-transformers>=2.2.0
pdfplumber>=0.10.0
easyocr>=1.7.0
```

### System Tools
```bash
# macOS
brew install poppler tesseract tesseract-lang w3m

# Python
pip install playwright arxiv sentence-transformers pdfplumber
playwright install chromium
```

### Emacs Packages
```elisp
(use-package pdf-tools)
(use-package shrface)
(use-package ace-link)
```

---

## Security Considerations

1. **Browser2 (Chrome) Access**
   - Always require explicit user permission
   - Log all actions to audit trail
   - Never auto-approve navigation to sensitive sites
   - Warn user when accessing financial/email sites

2. **Memory Storage**
   - Encrypt sensitive memories
   - Never store passwords/tokens in plain text
   - Add `.hikettei/` to `.gitignore` by default

3. **Research Data**
   - Respect rate limits on APIs
   - Cache results to minimize requests
   - Attribute sources properly

---

## Success Metrics

| Metric | Target |
|--------|--------|
| Memory retrieval latency | < 100ms |
| Research report generation | < 5 minutes for typical query |
| Chrome action permission UX | < 2 clicks to approve |
| Context preservation after compaction | > 90% relevant info retained |

---

## References

### Browser & Automation
- [Playwright MCP](https://github.com/microsoft/playwright-mcp)
- [Chrome DevTools MCP](https://developer.chrome.com/blog/chrome-devtools-mcp)
- [Browser Use MCP](https://docs.browser-use.com/customize/integrations/mcp-server)
- [shrface (EWW enhancement)](https://github.com/chenyanming/shrface)

### Deep Research
- [GPT Researcher](https://github.com/assafelovic/gpt-researcher)
- [LangChain Open Deep Research](https://github.com/langchain-ai/open_deep_research)
- [Tongyi DeepResearch](https://github.com/Alibaba-NLP/DeepResearch)
- [arXiv API](https://info.arxiv.org/help/api/index.html)

### Memory Systems
- [Mem0](https://github.com/mem0ai/mem0) | [Paper](https://arxiv.org/abs/2504.19413)
- [Cognee](https://github.com/topoteretes/cognee)
- [MCP Memory Service](https://github.com/doobidoo/mcp-memory-service)
- [Memory Survey Paper](https://arxiv.org/abs/2512.13564)

### PDF & OCR
- [pdf-tools](https://github.com/vedang/pdf-tools)
- [Tesseract OCR](https://github.com/tesseract-ocr/tesseract)
- [poppler-utils](https://poppler.freedesktop.org/)

---

## Timeline Summary

| Phase | Duration | Deliverables |
|-------|----------|--------------|
| Phase 1: Foundation | 2-3 weeks | Memory system, Memory panel |
| Phase 2: Browser | 2-3 weeks | Browser1 (EWW), Browser2 (Chrome) |
| Phase 3: Research | 2-3 weeks | Deep Research agent, Git tools |
| Phase 4: Documents | 1-2 weeks | PDF pipeline, OCR integration |
| Phase 5: Polish | 1-2 weeks | Integration, UI polish |
| **Total** | **8-13 weeks** | Complete system |

---

*Document created: 2025-01-11*
*Last updated: 2025-01-11*
