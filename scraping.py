import os, re, time, requests
from bs4 import BeautifulSoup
import pandas as pd
from urllib.parse import quote_plus
from datetime import datetime

# ディレクトリ・ファイル設定
HTML_DIR   = "html_data_all"
OUTPUT_CSV = "suumo_mansions_final.csv"

# CSV のカラム定義
COLUMNS = [
    "区名",
    "物件名",
    "販売価格 (万円)",
    "間取り",
    "専有面積 (m2)",
    "バルコニー (m2)",
    "築年月",
    "最寄駅",
    "最寄駅までの徒歩分数",
    "東京駅までの分数"
]

BASE_PRINT_URL = "https://transit.yahoo.co.jp/search/print"

def extract_station_name(text: str) -> str:
    """
    鉤括弧「…」内の駅名を返す。なければ「→」以前を返す。

    Args:
        text (str): 駅名を含むテキスト

    Returns:
        str: 抽出された駅名
    """
    m = re.search(r'「([^」]+)」', str(text))
    if m:
        return m.group(1)
    return re.split(r'→', str(text))[0].strip()

def extract_listing_info(soup: BeautifulSoup, ku_name: str) -> list[dict]:
    """
    1ページ分の物件情報を抽出して辞書のリストで返す。

    Args:
        soup (BeautifulSoup): パース済みのHTML全体オブジェクト
        ku_name (str): 区名

    Returns:
        list[dict]: 物件情報の辞書リスト
    """
    # 物件を全て取得
    listings = soup.select("div.property_unit")
    print(f"{ku_name} の物件数: {len(listings)}")
    data = []
    for item in listings:
        # <dt>/<dd>ペアを key→value に
        try:
            info_dict = {}
            for dl in item.select("div.dottable-line dl"):
                dt = dl.select_one("dt")
                dd = dl.select_one("dd")
                if dt and dd:
                    info_dict[dt.get_text(strip=True)] = dd.get_text(strip=True)

            # 各フィールドを取得
            name         = info_dict.get("物件名", "")
            price        = info_dict.get("販売価格", "")
            station_full = info_dict.get("沿線・駅", "")
            size         = info_dict.get("専有面積", "")
            layout       = info_dict.get("間取り", "")
            balcony      = info_dict.get("バルコニー", "")
            built        = info_dict.get("築年月", "")

            # 徒歩X分からXだけを抽出
            walk = ""
            station = station_full
            # 「徒歩」または「歩」が含まれているかチェックして分離
            if "徒歩" in station_full:
                parts = station_full.split("徒歩", 1)
                station = parts[0].strip()
                walk = "徒歩" + parts[1].strip()
            elif "歩" in station_full:
                parts = station_full.split("歩", 1)
                station = parts[0].strip()
                walk = "歩" + parts[1].strip()

            # 「徒歩5分」または「歩5分」にマッチ
            m_walk = re.search(r'(徒歩|歩)(\d+)分', walk)
            walk_num = m_walk.group(2) if m_walk else None     
            # 販売価格を「万」単位の整数に
            m_price = re.match(r'(?:(\d+)億)?(?:(\d+)万)?', price)
            oku = int(m_price.group(1)) if m_price.group(1) else 0
            man = int(m_price.group(2)) if m_price.group(2) else 0
            price_num = str(oku * 10000 + man)
            # バスX分などを削除して、駅名を分離
            m_station = re.search(r'「([^」]+)」', station_full)
            station = m_station.group(1) if m_station else None
            # m2を削除してfloat/intのみに
            m_size = re.search(r'([0-9]+(?:\\.[0-9]+)?)', str(size))
            m_balcony = re.search(r'([0-9]+(?:\\.[0-9]+)?)', str(balcony))
            size_final = float(m_size.group(1)) if m_size else None
            balcony_final = float(m_balcony.group(1)) if m_balcony else "なし"

            # 最終的なdict作成
            row = {
                "区名": ku_name,
                "物件名": name,
                "最寄駅": station,
                "販売価格 (万円)": price_num,
                "専有面積 (m2)": size_final,
                "バルコニー (m2)": balcony_final,
                "間取り": layout,
                "最寄駅までの徒歩分数": walk_num,               
                "築年月": built,
                # あとで埋める
                "東京駅までの分数": ""
            }
            data.append(row)
        except Exception as e:
            print(f"スキップされた物件（エラー: {e}）")
    return data

def get_commute_time(departure_station: str, destination_station: str = "東京駅") -> str:
    """
    Yahoo!路線情報から指定駅間の所要時間（分）を取得。

    Args:
        departure_station (str): 出発駅名
        destination_station (str): 目的駅名（デフォルトで東京駅）

    Returns:
        str: 所要時間（分）。取得失敗時は空文字。
    """
    route_url = (
        f"{BASE_PRINT_URL}?from={departure_station}"
        f"&to={destination_station}"
        f"&y=2025&m=07&d=09&hh=09&m1=0&m2=0&type=1&ticket=ic"
        "&expkind=1&userpass=1&ws=3&s=0&al=1&shin=1&ex=1&hb=1&lb=1&sr=1"
    )
    # HTTPリクエストを送信
    route_response = requests.get(route_url)
    route_soup = BeautifulSoup(route_response.text, 'html.parser')
    route_summary = route_soup.find("div",class_ = "routeSummary")
    # テキスト取得
    required_time = route_summary.find("li",class_ = "time").get_text()
    print(departure_station + "からの所要時間： " + required_time)
    # 整数に変換
    m = re.search(r"着(\d+)分\(", required_time)
    if m:
        print(m.group(1))
    return m.group(1) if m else ""

def main():
    """
    メイン処理：HTML解析、データフレーム化、所要時間取得、CSV出力までを実行。
    """
    all_data = []

    # 1) HTML から物件情報を抽出
    for fname in os.listdir(HTML_DIR):
        if not fname.endswith(".html"):
            continue
        base = fname.replace(".html", "")
        ku_name = re.sub(r'_\d+$', '', base)
        path = os.path.join(HTML_DIR, fname)
        print(f"解析中: {ku_name}")
        with open(path, "r", encoding="utf-8") as f:
            soup = BeautifulSoup(f, "html.parser")
        listings = extract_listing_info(soup, ku_name)
        all_data.extend(listings)

    # 2) DataFrame 化
    df = pd.DataFrame(all_data, columns=COLUMNS)

    # 3) 最寄駅ごとに所要時間を取得（キャッシュ使用）
    cache = {}
    # 同じ駅を複数回スクレイプするのを防ぐ
    for station in df["最寄駅"].unique():
        if not station or station in cache:
            cache.setdefault(station, "")
            continue
        query_name = station
        # 高野駅だけは岡山県の高野駅と誤認して不正確な値を吐き出すので、個別に修正
        if station == "高野":
            query_name = "高野(東京都)"
        print(f"Processing: {station} → 東京駅", end=" ")
        try:
            mins = get_commute_time(query_name)
            cache[station] = mins
            print(f"got: {mins}分")
        # バス停駅はYahoo APIの対象外なので記載
        except Exception as e:
            cache[station] = "バス停のみ"
            print(f"failed: バス停のみ")
        time.sleep(1)

    # 4) マージして CSV 出力
    df["東京駅までの分数"] = df["最寄駅"].map(cache)
    df = df.applymap(lambda x: x.replace('\xa0', ' ') if isinstance(x, str) else x)

    # CSV 出力 (Shift-JIS/CP932)
    df.to_csv(OUTPUT_CSV, index=False, encoding="cp932")
    print(f"\n出力完了: {OUTPUT_CSV}")

if __name__ == "__main__":
    main()
