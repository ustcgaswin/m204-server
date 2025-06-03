import streamlit as st
import os
import pandas as pd
import re
import html
from collections import defaultdict
import time
from st_keyup import st_keyup

# Page configuration
st.set_page_config(
    layout="wide", 
    page_title="Log Analytics Dashboard",
    page_icon="📊",
    initial_sidebar_state="expanded"
)

LOGS_DIR = os.path.join(os.path.dirname(__file__), "..", "logs")

# Custom CSS for professional styling and improved UX
st.markdown("""
<style>
    /* Import Google Fonts */
    @import url('https://fonts.googleapis.com/css2?family=Inter:wght@300;400;500;600;700&display=swap');
    
    /* Global styles */
    .main {
        padding: 1rem 2rem;
    }
    
    html, body, [class*="css"] {
        font-family: 'Inter', sans-serif;
    }
    
    /* Block container styling */
    .block-container {
        padding-top: 1rem;
        padding-bottom: 0rem;
        padding-left: 5rem;
        padding-right: 5rem;
    }
    
    /* Header styling */
    .main-header {
        padding: 1rem 0;
        margin-bottom: 1.5rem;
        border-bottom: 2px solid #e2e8f0;
    }
    
    .main-header h1 {
        font-size: 1.75rem;
        font-weight: 600;
        margin: 0;
        color: #1f2937;
    }
    
    /* Stats cards */
    .stats-container {
        display: grid;
        grid-template-columns: repeat(auto-fit, minmax(200px, 1fr));
        gap: 1rem;
        margin-bottom: 2rem;
    }
    
    .stat-card {
        background: white;
        padding: 1.5rem;
        border-radius: 12px;
        box-shadow: 0 4px 20px rgba(0,0,0,0.08);
        border: 1px solid #f0f2f6;
        transition: transform 0.2s ease, box-shadow 0.2s ease;
    }
    
    .stat-card:hover {
        transform: translateY(-2px);
        box-shadow: 0 8px 25px rgba(0,0,0,0.12);
    }
    
    .stat-value {
        font-size: 2rem;
        font-weight: 700;
        margin: 0;
        line-height: 1;
    }
    
    .stat-label {
        color: #64748b;
        font-size: 0.875rem;
        font-weight: 500;
        margin: 0.25rem 0 0 0;
        text-transform: uppercase;
        letter-spacing: 0.05em;
    }
    
    /* File selection styling */
    .file-selector {
        background: white;
        padding: 1rem;
        border-radius: 8px;
        border: 1px solid #e2e8f0;
        margin-bottom: 1rem;
    }
    
    .selected-file-info {
        background: #f0f9ff;
        padding: 0.75rem;
        border-radius: 6px;
        border: 1px solid #0ea5e9;
        margin-top: 0.5rem;
    }
    
    .file-name {
        font-weight: 600;
        color: #0f172a;
        font-size: 0.875rem;
    }
    
    .file-meta {
        font-size: 0.75rem;
        color: #64748b;
        margin-top: 0.25rem;
    }
    
    /* Search styling with live indicator */
    .search-container {
        position: relative;
        margin-bottom: 1rem;
    }
    
    .search-indicator {
        position: absolute;
        right: 8px;
        top: 50%;
        transform: translateY(-50%);
        font-size: 0.75rem;
        color: #6b7280;
        background: white;
        padding: 0 4px;
    }
    
    .search-indicator.searching {
        color: #3b82f6;
    }
    
    /* Log entry cards */
    .log-entry {
        background: white;
        border: 1px solid #e2e8f0;
        border-radius: 8px;
        margin-bottom: 0.75rem;
        overflow: hidden;
        transition: all 0.2s ease;
        box-shadow: 0 1px 3px rgba(0,0,0,0.05);
    }
    
    .log-entry:hover {
        box-shadow: 0 4px 12px rgba(0,0,0,0.1);
        border-color: #cbd5e1;
    }
    
    .log-header {
        background: #f8fafc;
        padding: 0.75rem 1rem;
        border-bottom: 1px solid #e2e8f0;
        display: flex;
        justify-content: space-between;
        align-items: center;
        font-size: 0.875rem;
    }
    
    .log-timestamp {
        font-weight: 600;
        color: #374151;
        font-family: 'Monaco', 'Consolas', monospace;
    }
    
    .log-level-badge {
        padding: 0.25rem 0.75rem;
        border-radius: 20px;
        font-weight: 600;
        font-size: 0.75rem;
        text-transform: uppercase;
        letter-spacing: 0.05em;
    }
    
    .log-meta {
        color: #6b7280;
        font-size: 0.8rem;
    }
    
    .log-content {
        padding: 1rem;
    }
    
    .log-logger {
        color: #6366f1;
        font-weight: 500;
        font-size: 0.875rem;
        margin-bottom: 0.5rem;
    }
    
    .log-message {
        font-family: 'Monaco', 'Consolas', monospace;
        font-size: 0.875rem;
        line-height: 1.6;
        color: #1f2937;
        white-space: pre-wrap;
        word-wrap: break-word;
        background: #f9fafb;
        padding: 0.75rem;
        border-radius: 6px;
        border-left: 3px solid #e5e7eb;
    }
    
    /* Level-specific styling */
    .level-DEBUG .log-level-badge {
        background: #dcfce7;
        color: #166534;
    }
    
    .level-INFO .log-level-badge {
        background: #dbeafe;
        color: #1e40af;
    }
    
    .level-INFO .log-message {
        border-left-color: #3b82f6;
    }
    
    .level-WARNING .log-level-badge {
        background: #fef3c7;
        color: #92400e;
    }
    
    .level-WARNING .log-message {
        border-left-color: #f59e0b;
    }
    
    .level-ERROR .log-level-badge {
        background: #fee2e2;
        color: #b91c1c;
    }
    
    .level-ERROR .log-message {
        border-left-color: #ef4444;
    }
    
    .level-CRITICAL .log-level-badge {
        background: #fecaca;
        color: #7f1d1d;
        animation: pulse 2s infinite;
    }
    
    .level-CRITICAL .log-message {
        border-left-color: #dc2626;
        background: #fef2f2;
    }
    
    .level-UNKNOWN .log-level-badge {
        background: #f3f4f6;
        color: #4b5563;
    }
            
    .main-search {
    background: white;
    padding: 1.5rem;
    border-radius: 12px;
    margin-bottom: 2rem;
    box-shadow: 0 4px 20px rgba(0,0,0,0.08);
    border: 2px solid #e2e8f0;
}

.main-search:focus-within {
    border-color: #6366f1;
    box-shadow: 0 4px 20px rgba(99, 102, 241, 0.2);
}

/* Search input improvements */
.stTextInput > div > div > input {
    border-radius: 8px;
    border: 2px solid #e2e8f0;
    padding: 0.75rem 1rem;
    font-size: 1rem;
    background: #fafafa;
}

.stTextInput > div > div > input:focus {
    border-color: #6366f1 !important;
    box-shadow: 0 0 0 3px rgba(99, 102, 241, 0.1) !important;
    background: white;
}
    
    /* Highlight styling */
    mark {
        background: linear-gradient(120deg, #fbbf24 0%, #f59e0b 100%);
        color: #92400e;
        padding: 0.1em 0.3em;
        border-radius: 3px;
        font-weight: 600;
    }
    
    /* Sidebar styling */
    .css-1d391kg {
        background-color: #f8fafc;
    }
    
    /* Filter section styling */
    .filter-section {
        background: white;
        padding: 1.5rem;
        border-radius: 8px;
        margin-bottom: 1rem;
        box-shadow: 0 1px 3px rgba(0,0,0,0.1);
    }
    
    /* Search box styling */
    .stTextInput > div > div > input {
        border-radius: 8px;
        border: 2px solid #e2e8f0;
        padding: 0.75rem;
        font-size: 0.875rem;
    }
    
    .stTextInput > div > div > input:focus {
        border-color: #6366f1;
        box-shadow: 0 0 0 3px rgba(99, 102, 241, 0.1);
    }
    
    /* Pagination styling */
    .pagination-container {
        display: flex;
        justify-content: center;
        align-items: center;
        padding: 2rem 0;
        background: white;
        border-radius: 8px;
        margin-top: 2rem;
        box-shadow: 0 1px 3px rgba(0,0,0,0.1);
    }
    
    /* Empty state styling */
    .empty-state {
        text-align: center;
        padding: 4rem 2rem;
        color: #6b7280;
    }
    
    .empty-state-icon {
        font-size: 4rem;
        margin-bottom: 1rem;
    }
    
    /* Animation for critical logs */
    @keyframes pulse {
        0%, 100% {
            opacity: 1;
        }
        50% {
            opacity: 0.7;
        }
    }
    
    /* Quick action buttons */
    .quick-actions {
        display: flex;
        gap: 0.5rem;
        margin-bottom: 1rem;
        flex-wrap: wrap;
    }
    
    .quick-action-btn {
        background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
        color: white;
        border: none;
        border-radius: 6px;
        padding: 0.5rem 1rem;
        font-size: 0.75rem;
        font-weight: 500;
        cursor: pointer;
        transition: all 0.2s ease;
    }
    
    .quick-action-btn:hover {
        transform: translateY(-1px);
        box-shadow: 0 4px 12px rgba(102, 126, 234, 0.4);
    }
    
    /* Responsive design */
    @media (max-width: 768px) {
        .main {
            padding: 0.5rem 1rem;
        }
        
        .block-container {
            padding-left: 1rem;
            padding-right: 1rem;
        }
        
        .log-header {
            flex-direction: column;
            align-items: flex-start;
            gap: 0.5rem;
        }
        
        .stats-container {
            grid-template-columns: repeat(2, 1fr);
        }
    }
</style>
""", unsafe_allow_html=True)

@st.cache_data
def get_log_files():
    """Scans the LOGS_DIR for .log files and organizes by folder."""
    if not os.path.exists(LOGS_DIR):
        return {}
    
    files_by_folder = defaultdict(list)
    
    for root, dirs, files in os.walk(LOGS_DIR):
        log_files = [f for f in files if f.endswith(".log")]
        if log_files:
            folder_name = os.path.relpath(root, LOGS_DIR)
            if folder_name == ".":
                folder_name = "Root"
            
            for file in log_files:
                file_path = os.path.join(root, file)
                file_info = {
                    'name': file,
                    'path': os.path.relpath(file_path, LOGS_DIR),
                    'full_path': file_path,
                    'size': os.path.getsize(file_path),
                    'modified': os.path.getmtime(file_path)
                }
                files_by_folder[folder_name].append(file_info)
    
    # Sort files by modification time within each folder
    for folder in files_by_folder:
        files_by_folder[folder].sort(key=lambda x: x['modified'], reverse=True)
    
    return dict(files_by_folder)

def parse_log_line(line):
    """
    Parses a single log line.
    Format: %(asctime)s - %(levelname)s - [%(filename)s:%(lineno)d] - %(name)s - %(message)s
    """
    match = re.match(r"(\d{4}-\d{2}-\d{2} \d{2}:\d{2}:\d{2},\d{3}) - (\w+) - \[(.*?):(\d+)\] - (.*?) - (.*)", line)
    if match:
        return {
            "Timestamp": pd.to_datetime(match.group(1), format="%Y-%m-%d %H:%M:%S,%f"),
            "Level": match.group(2),
            "File": match.group(3),
            "Line": int(match.group(4)),
            "LoggerName": match.group(5),
            "Message": match.group(6).strip()
        }
    return None

@st.cache_data
def load_log_data(selected_file):
    """Loads and parses log data from a single selected file."""
    all_log_entries = []
    
    full_path = os.path.join(LOGS_DIR, selected_file)
    try:
        with open(full_path, "r", encoding="utf-8") as f:
            for line_num, line in enumerate(f, 1):
                parsed_line = parse_log_line(line)
                if parsed_line:
                    parsed_line["SourceFile"] = selected_file
                    parsed_line["OriginalLineNum"] = line_num
                    all_log_entries.append(parsed_line)
                else:
                    try:
                        ts_match = re.search(r"(\d{4}-\d{2}-\d{2} \d{2}:\d{2}:\d{2}(?:,\d{3})?)", line)
                        timestamp = pd.to_datetime(ts_match.group(1)) if ts_match else pd.NaT
                    except (AttributeError, ValueError, TypeError):
                        timestamp = pd.NaT
                    all_log_entries.append({
                        "Timestamp": timestamp, "Level": "UNKNOWN", "File": "N/A",
                        "Line": 0, "LoggerName": "N/A", "Message": line.strip(),
                        "SourceFile": selected_file, "OriginalLineNum": line_num
                    })
    except Exception as e:
        st.error(f"Error reading {selected_file}: {e}")
        return pd.DataFrame()
    
    if not all_log_entries:
        return pd.DataFrame()

    df = pd.DataFrame(all_log_entries)
    df["Timestamp"] = pd.to_datetime(df["Timestamp"], errors='coerce')
    return df

def highlight_term(text, term):
    """Highlights the search term in the text using <mark> tags."""
    if not term:
        return html.escape(text)
    
    escaped_text = html.escape(text)
    try:
        highlighted_text = re.sub(f"({re.escape(term)})", r"<mark>\1</mark>", escaped_text, flags=re.IGNORECASE)
    except re.error:
        return escaped_text
    return highlighted_text

def create_stats_dashboard(df):
    """Creates a statistics dashboard for the logs."""
    if df.empty:
        return
    
    # Level distribution
    level_counts = df['Level'].value_counts()
    total_logs = len(df)
    
    # Create stats cards
    col1, col2, col3, col4 = st.columns(4)
    
    with col1:
        st.markdown(f"""
        <div class="stat-card">
            <div class="stat-value" style="color: #3b82f6;">{total_logs:,}</div>
            <div class="stat-label">Total Entries</div>
        </div>
        """, unsafe_allow_html=True)
    
    with col2:
        error_count = level_counts.get('ERROR', 0) + level_counts.get('CRITICAL', 0)
        st.markdown(f"""
        <div class="stat-card">
            <div class="stat-value" style="color: #ef4444;">{error_count:,}</div>
            <div class="stat-label">Errors & Critical</div>
        </div>
        """, unsafe_allow_html=True)
    
    with col3:
        warning_count = level_counts.get('WARNING', 0)
        st.markdown(f"""
        <div class="stat-card">
            <div class="stat-value" style="color: #f59e0b;">{warning_count:,}</div>
            <div class="stat-label">Warnings</div>
        </div>
        """, unsafe_allow_html=True)
    
    with col4:
        info_count = level_counts.get('INFO', 0)
        st.markdown(f"""
        <div class="stat-card">
            <div class="stat-value" style="color: #10b981;">{info_count:,}</div>
            <div class="stat-label">Info Messages</div>
        </div>
        """, unsafe_allow_html=True)

def render_file_selector(files_by_folder):
    """Renders a single file selector with quick access options."""
    st.markdown("### 📁 Select Log File")
    
    # Create a flat list of all files with folder context
    all_files = []
    file_options = []
    
    for folder_name, files in files_by_folder.items():
        for file_info in files:
            all_files.append(file_info)
            # Create display name - remove "Root /" prefix and clean up display
            if folder_name == "Root":
                display_name = f"📄 {file_info['name']} ({file_info['size']/1024:.1f} KB)"
            else:
                display_name = f"📂 {folder_name} / 📄 {file_info['name']} ({file_info['size']/1024:.1f} KB)"
            file_options.append(display_name)
    
    if not all_files:
        st.warning("No log files found")
        return None
    
    # Quick access button (only Latest now)
    latest_file = st.button("🕒 Latest", help="Select most recent log file", use_container_width=True)
    
    # Initialize session state for selected file
    if 'selected_file_index' not in st.session_state:
        st.session_state.selected_file_index = 0
    
    # Handle quick access button
    if latest_file:
        # Find most recent file
        latest_idx = max(range(len(all_files)), key=lambda i: all_files[i]['modified'])
        st.session_state.selected_file_index = latest_idx
        st.rerun()
    
    # File selector dropdown
    selected_index = st.selectbox(
        "Choose a log file:",
        options=range(len(file_options)),
        format_func=lambda x: file_options[x],
        index=st.session_state.selected_file_index,
        key="file_selector"
    )
    
    # Update session state
    st.session_state.selected_file_index = selected_index
    
    # Show selected file info
    selected_file_info = all_files[selected_index]
    st.markdown(f"""
    <div class="selected-file-info">
        <div class="file-name">✅ Selected: {selected_file_info['name']}</div>
        <div class="file-meta">
            📂 Path: {selected_file_info['path']}<br>
            💾 Size: {selected_file_info['size']/1024:.1f} KB<br>
            🕒 Modified: {time.strftime('%Y-%m-%d %H:%M:%S', time.localtime(selected_file_info['modified']))}
        </div>
    </div>
    """, unsafe_allow_html=True)
    
    return selected_file_info['path']


def filter_dataframe_live(df, search_term, selected_levels, selected_source_files):
    """Filters dataframe with live search capabilities."""
    if df.empty:
        return df
    
    filtered_df = df.copy()
    
    # Live search filter
    if search_term:
        # Create a combined search field for faster searching
        search_columns = ['Message', 'LoggerName', 'File']
        search_mask = pd.Series([False] * len(filtered_df))
        
        for col in search_columns:
            if col in filtered_df.columns:
                mask = filtered_df[col].astype(str).str.contains(search_term, case=False, na=False)
                search_mask = search_mask | mask
        
        filtered_df = filtered_df[search_mask]
    
    # Level filter - FIXED: Handle empty list correctly
    if selected_levels is not None:
        if len(selected_levels) == 0:
            # If no log levels are selected, return empty dataframe
            return pd.DataFrame()
        else:
            # Filter by selected log levels
            filtered_df = filtered_df[filtered_df["Level"].isin(selected_levels)]
    
    # Source file filter - FIXED: Handle empty list correctly
    if selected_source_files is not None:
        if len(selected_source_files) == 0:
            # If no source files are selected, return empty dataframe
            return pd.DataFrame()
        else:
            # Filter by selected source files
            filtered_df = filtered_df[filtered_df["File"].isin(selected_source_files)]
    
    return filtered_df

# Main application


def main():
    # Header
    st.markdown("""
    <div class="main-header">
        <h1>📊 Log Viewer</h1>
    </div>
    """, unsafe_allow_html=True)

    # Improved keyboard shortcut handler
    st.html("""
    <script>
    document.addEventListener('DOMContentLoaded', function() {
        let searchInput = null;
        
        function findSearchInput() {
            // Try multiple selectors to find the search input
            const selectors = [
                'input[placeholder*="Start typing to search"]',
                'input[aria-label*="Search"]',
                '.stTextInput input',
                'input[key="live_search_main"]'
            ];
            
            for (const selector of selectors) {
                const input = document.querySelector(selector);
                if (input) {
                    searchInput = input;
                    return input;
                }
            }
            return null;
        }
        
        function setupKeyboardShortcuts() {
            document.addEventListener('keydown', function(event) {
                // Ctrl+Enter for search focus
                if (event.ctrlKey && event.key === 'Enter') {
                    event.preventDefault();
                    const input = findSearchInput();
                    if (input) {
                        input.focus();
                        input.select();
                    }
                    return;
                }
                
                // Escape to clear search (when search is focused)
                if (event.key === 'Escape') {
                    const input = findSearchInput();
                    if (input && document.activeElement === input) {
                        input.value = '';
                        // Trigger change event for Streamlit
                        input.dispatchEvent(new Event('input', { bubbles: true }));
                        input.dispatchEvent(new Event('change', { bubbles: true }));
                        input.blur();
                    }
                    return;
                }
            });
        }
        
        // Setup shortcuts initially
        setupKeyboardShortcuts();
        
        // Re-setup shortcuts after Streamlit updates
        const observer = new MutationObserver(function(mutations) {
            mutations.forEach(function(mutation) {
                if (mutation.addedNodes.length > 0) {
                    // Reset search input reference
                    searchInput = null;
                    setTimeout(setupKeyboardShortcuts, 100);
                }
            });
        });
        
        observer.observe(document.body, {
            childList: true,
            subtree: true
        });
    });
    </script>
    """)

    # Check if logs directory exists
    if not os.path.exists(LOGS_DIR):
        st.error(f"📁 Logs directory not found: `{LOGS_DIR}`")
        st.info("Please ensure the logs directory exists relative to the script.")
        return

    files_by_folder = get_log_files()

    if not files_by_folder:
        st.markdown("""
        <div class="empty-state">
            <div class="empty-state-icon">📄</div>
            <h3>No log files found</h3>
            <p>No .log files found in the logs directory.</p>
        </div>
        """, unsafe_allow_html=True)
        return

    # Live Search at the top of main content
    st.markdown("""
    <div class="main-search">
    """, unsafe_allow_html=True)
    
    st.markdown("### 🔍 Live Search")

    # Initialize search term in session state if not exists
    if 'search_term' not in st.session_state:
        st.session_state.search_term = ""

    # Use columns for better layout - adjusted ratios for better clear button positioning
    col1, col2 = st.columns([5, 1])

    with col1:
        # Live search that triggers on every keystroke
        try:
            search_term = st_keyup(
                "Search across all log messages, loggers, and files",
                value=st.session_state.search_term,
                placeholder="Start typing to search instantly... (Ctrl+Enter to focus)",
                help="Search is case-insensitive and searches across messages, loggers, and source files",
                key="live_search_main",
                debounce=300  # Add small delay to prevent excessive updates
            )
        except Exception:
            # Fallback to regular text input if st_keyup fails
            search_term = st.text_input(
                "Search across all log messages, loggers, and files",
                value=st.session_state.search_term,
                placeholder="Type and press Enter to search... (Ctrl+Enter to focus)",
                help="Search is case-insensitive and searches across messages, loggers, and source files",
                key="fallback_search_main"
            )
        
        # Update session state
        if search_term != st.session_state.search_term:
            st.session_state.search_term = search_term

    with col2:
        # Add some spacing to align the clear button with the input field
        st.markdown("<br>", unsafe_allow_html=True)
        # Clear search button
        if st.button("🗑️ Clear", help="Clear search", use_container_width=True, key="clear_search_btn"):
            st.session_state.search_term = ""
            st.rerun()

    # Search statistics
    if search_term:
        st.caption(f"🔍 **Active search:** `{search_term}` • Press **Escape** to clear")
    else:
        st.caption("💡 **Tip:** Use **Ctrl+Enter** to focus search bar • Search across messages, loggers, and files")

    st.markdown("""
    </div>
    """, unsafe_allow_html=True)

    # Sidebar filters
    with st.sidebar:
        st.markdown("## 🎛️ Controls")
        
        # Update keyboard shortcuts info
        st.markdown("""
        <div style="background: #f0f9ff; padding: 0.5rem; border-radius: 6px; margin-bottom: 1rem; border: 1px solid #0ea5e9;">
            <div style="font-size: 0.75rem; color: #0f172a; font-weight: 500;">⌨️ Keyboard Shortcuts</div>
            <div style="font-size: 0.7rem; color: #64748b; margin-top: 0.25rem;">
                <code>Ctrl+Enter</code> → Focus search bar<br>
                <code>Escape</code> → Clear search (when focused)
            </div>
        </div>
        """, unsafe_allow_html=True)
        
        # Single file selection with improved UX
        selected_file = render_file_selector(files_by_folder)
        
        if not selected_file:
            st.warning("⚠️ Please select a log file.")
            return
        
        st.markdown("---")
        
        # Load data first to get available options
        log_df = load_log_data(selected_file) if selected_file else pd.DataFrame()
        
        # Quick filter buttons (moved to top)
        st.markdown("### ⚡ Quick Filters")
        col1, col2 = st.columns(2)
        
        # Initialize session state for quick filters
        if 'quick_filter_applied' not in st.session_state:
            st.session_state.quick_filter_applied = None
        
        show_errors_only = col1.button("🚨 Errors", help="Show only errors and critical", use_container_width=True)
        show_warnings = col2.button("⚠️ Warnings", help="Show warnings and above", use_container_width=True)
        
        st.markdown("---")
        
        # Log Levels Filter
        st.markdown("### 📊 Log Levels")
        if not log_df.empty:
            available_levels = sorted(log_df["Level"].unique())
            
            # Initialize log levels selection in session state
            if 'log_levels_selection' not in st.session_state:
                st.session_state.log_levels_selection = available_levels
            
            # Quick selection buttons for log levels
            col1, col2 = st.columns(2)
            select_all_levels = col1.button("✅ All", help="Select all log levels", use_container_width=True, key="select_all_levels")
            clear_levels = col2.button("❌ Clear", help="Clear log level selection", use_container_width=True, key="clear_levels")
            
            # Apply quick filters if buttons were pressed
            if show_errors_only:
                default_levels = [level for level in available_levels if level in ['ERROR', 'CRITICAL']]
                st.session_state.quick_filter_applied = 'errors'
                st.session_state.log_levels_selection = default_levels
            elif show_warnings:
                default_levels = [level for level in available_levels if level in ['WARNING', 'ERROR', 'CRITICAL']]
                st.session_state.quick_filter_applied = 'warnings'
                st.session_state.log_levels_selection = default_levels
            elif select_all_levels:
                st.session_state.log_levels_selection = available_levels
                st.session_state.quick_filter_applied = None
                st.rerun()
            elif clear_levels:
                st.session_state.log_levels_selection = []
                st.session_state.quick_filter_applied = None
                st.rerun()
            else:
                # Check if we should maintain previous quick filter
                if st.session_state.quick_filter_applied == 'errors':
                    default_levels = [level for level in available_levels if level in ['ERROR', 'CRITICAL']]
                    if st.session_state.log_levels_selection != default_levels:
                        st.session_state.log_levels_selection = default_levels
                elif st.session_state.quick_filter_applied == 'warnings':
                    default_levels = [level for level in available_levels if level in ['WARNING', 'ERROR', 'CRITICAL']]
                    if st.session_state.log_levels_selection != default_levels:
                        st.session_state.log_levels_selection = default_levels
            
            selected_levels = st.multiselect(
                "Filter by log level:",
                options=available_levels,
                default=st.session_state.log_levels_selection,
                help="Filter by log severity level",
                label_visibility="collapsed",
                key="log_levels_filter"
            )
            
            # Update session state with current selection
            st.session_state.log_levels_selection = selected_levels
        else:
            selected_levels = []
        
        st.markdown("---")
        
        # Source Code Files Filter with select all/clear options
        st.markdown("### 📄 Source Code Files")
        if not log_df.empty:
            available_source_files = sorted(log_df["File"].unique())
            
            # Initialize source files selection in session state
            if 'source_files_selection' not in st.session_state:
                st.session_state.source_files_selection = available_source_files
            
            # Quick selection buttons for source files
            col1, col2 = st.columns(2)
            select_all_sources = col1.button("✅ All", help="Select all source files", use_container_width=True, key="select_all_sources")
            clear_sources = col2.button("❌ Clear", help="Clear source file selection", use_container_width=True, key="clear_sources")
            
            # Handle quick selection buttons
            if select_all_sources:
                st.session_state.source_files_selection = available_source_files
                st.rerun()
            elif clear_sources:
                st.session_state.source_files_selection = []
                st.rerun()
            
            selected_source_files = st.multiselect(
                "Filter by source file:",
                options=available_source_files,
                default=st.session_state.source_files_selection,
                help="Filter by source code file that generated the log",
                label_visibility="collapsed",
                key="source_files_filter"
            )
            
            # Update session state with current selection
            st.session_state.source_files_selection = selected_source_files
        else:
            selected_source_files = []
        
        st.markdown("---")
        
        # Display options
        st.markdown("### 📋 Display Options")
        
        sort_order = st.radio(
            "🔄 Sort Order",
            options=["Newest First", "Oldest First"],
            index=0,
            help="Choose timestamp sorting order"
        )
        
        page_size = st.selectbox(
            "📄 Items per Page",
            options=[10, 25, 50, 100, 200],
            index=2,
            help="Number of log entries to show per page"
        )

    # Load and process data
    if not selected_file:
        return
    
    # Load data (we already loaded it above for filters)
    if log_df.empty:
        st.warning("📊 Selected log file is empty or could not be parsed.")
        return

    # Apply filters with live search
    filtered_df = filter_dataframe_live(log_df, search_term, selected_levels, selected_source_files)

    # Apply sorting
    if "Timestamp" in filtered_df.columns and not filtered_df.empty:
        filtered_df = filtered_df.sort_values(
            by="Timestamp", 
            ascending=(sort_order == "Oldest First"), 
            na_position='last'
        )

    # Show statistics
    create_stats_dashboard(filtered_df)

    # Results section
    st.markdown("---")
    
    if filtered_df.empty:
        st.markdown("""
        <div class="empty-state">
            <div class="empty-state-icon">🔍</div>
            <h3>No matching entries</h3>
            <p>Try adjusting your search criteria or filters.</p>
        </div>
        """, unsafe_allow_html=True)
        return

    # Results header with live count
    result_count = len(filtered_df)
    total_count = len(log_df)
    
    col1, col2 = st.columns([3, 1])
    with col1:
        if search_term:
            st.subheader(f"🔍 Search Results: {result_count:,} of {total_count:,} entries")
        else:
            st.subheader(f"📋 Log Entries: {result_count:,} of {total_count:,}")
    
    with col2:
        if search_term and result_count > 0:
            st.metric("Match Rate", f"{(result_count/total_count)*100:.1f}%")
    
    # Pagination
    total_pages = (result_count - 1) // page_size + 1 if result_count > 0 else 1
    
    col1, col2, col3 = st.columns([1, 2, 1])
    with col2:
        current_page = st.number_input(
            "Page",
            min_value=1,
            max_value=total_pages,
            value=1,
            help=f"Navigate through {total_pages} pages"
        )

    # Display paginated results
    start_idx = (current_page - 1) * page_size
    end_idx = start_idx + page_size
    paginated_df = filtered_df.iloc[start_idx:end_idx]

    for _, row in paginated_df.iterrows():
        ts_str = row['Timestamp'].strftime('%Y-%m-%d %H:%M:%S.%f')[:-3] if pd.notna(row['Timestamp']) else "No Timestamp"
        level = row['Level']
        message_html = highlight_term(str(row['Message']), search_term)
        logger_html = highlight_term(str(row['LoggerName']), search_term)
        file_html = highlight_term(str(row['File']), search_term)

        st.markdown(f"""
        <div class="log-entry level-{level}">
            <div class="log-header">
                <div>
                    <span class="log-timestamp">{ts_str}</span>
                    <span class="log-level-badge">{level}</span>
                </div>
                <div class="log-meta">
                    {file_html}:{row['Line']} • {html.escape(row['SourceFile'])}
                </div>
            </div>
            <div class="log-content">
                <div class="log-logger">Logger: {logger_html}</div>
                <div class="log-message">{message_html}</div>
            </div>
        </div>
        """, unsafe_allow_html=True)

    # Pagination info
    if total_pages > 1:
        st.markdown(f"""
        <div class="pagination-container">
            <span>Page {current_page} of {total_pages} • Showing {len(paginated_df)} of {result_count:,} entries</span>
        </div>
        """, unsafe_allow_html=True)

if __name__ == "__main__":
    main()

