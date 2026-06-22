package cs305.minesweeper.winmine;

import java.awt.Color;
import java.awt.Dimension;
import java.util.ArrayList;
import java.util.HashSet;

import cs305.minesweeper.MinesweeperTile;
import cs305.minesweeper.MinesweeperBoard.GameState;

/**
 * 
 * @author Michael Sergio
 *
 */
public class WinMineTile implements MinesweeperTile {

	private static final int CENTER_OFFSET = 0;
	private static final Color ONE 		= new Color(0,0,255);	// blue
	private static final Color TWO 		= new Color(0,128,0);	// green
	private static final Color THREE 	= new Color(255,0,0);	// red
	private static final Color FOUR		= new Color(0,0,128);	// purple
	private static final Color FIVE		= new Color(128,0,0);	// burgundy 
	private static final Color SIX 		= new Color(0,128,128);	// torquise
	private static final Color SEVEN 	= new Color(0,0,0);		// black
	private static final Color EIGHT 	= new Color(128,128,128);//gray, the bad kind 

	int xPos;
	int yPos;
	
	int value = UNKNOWN_VALUE;

	WinMineBoard board;

	public WinMineTile(int x, int y, WinMineBoard board) {

		xPos = x;
		yPos = y;
		this.board = board;
	}

	public static void main(String[] args) {
		WinMineBoard board = new WinMineBoard();

		LOOP:
			for (int y = 0; y < board.theGame.length ; y++) {
				for (int x  = 0; x < board.theGame[y].length; x++) {	
					board.clickTile(board.theGame[x][y]);
					System.out.println(board.theGame[x][y].detectColorsOfInside());
					if (board.getGameState() == GameState.Lost)
						break LOOP;
				}
			}



	}

	@Override
	public TileState getState() {
		
		if (value == UNKNOWN_VALUE)
			return TileState.Hidden;
		else if (value == FLAG_VALUE)
			return TileState.Flagged;
		else
			return TileState.Exposed;
	}

	public Dimension getCenterPoint() {

		int centerX = board.offsetCorner.width + board.tileSize.width * (xPos) + (board.tileSize.width / 2) ;
		int centerY =  board.offsetCorner.height + board.tileSize.height * (yPos) + (board.tileSize.height / 2) ;
		return new Dimension(centerX + CENTER_OFFSET, centerY + CENTER_OFFSET); // offset to take into account the gray line
	}

	@Override
	public MinesweeperTile[] getSurroundingTiles() {

		ArrayList<MinesweeperTile> surrondingList = new ArrayList<MinesweeperTile>(8);
		for (int x = xPos - 1 ; x <= xPos + 1; x++) {
			for (int y = yPos - 1 ; y <= yPos + 1; y++) {
				if (x >= 0 && y >= 0 && x < board.amountOfTiles.width && y < board.amountOfTiles.height && !(xPos == x && yPos == y))
					surrondingList.add(board.theGame[x][y]);
			}
		}

		MinesweeperTile[] tiles = new MinesweeperTile[surrondingList.size()];
		int tileIndex = 0;
		for (MinesweeperTile tile : surrondingList){
			tiles[tileIndex++] = tile;
		}

		return tiles;
	}

	@Override
	public int getValue() {
		return value;
	}
	
	public int scanForValue() {

		// cut out the inside check for how many colors there are
		HashSet<Color> colors = detectColorsOfInside();

		// 1 is empty
		// 2 is gray and a number color
		// 3 is flag or bomb, need to check color to deteremine

		switch (colors.size()) {
		case 1: return EMPTY_VALUE;
		case 2: 
			for (Color c : colors) {
				if (c.equals(ONE)) 	return 1;
				if (c.equals(TWO)) 	return 2;
				if (c.equals(THREE))return 3;
				if (c.equals(FOUR)) return 4;
				if (c.equals(FIVE)) return 5;
				if (c.equals(SIX)) 	return 6;
				if (c.equals(SEVEN))return 7;
				if (c.equals(EIGHT))return 8;
			}
			return UNKNOWN_VALUE;
		case 3: return BOMB_VALUE;
		case 4:	return FLAG_VALUE;
		default:
			return UNKNOWN_VALUE;


		}
	}

	private HashSet<Color> detectColorsOfInside() {
		HashSet<Color> colors = new HashSet<Color>();

		Dimension centerPoint = getCenterPoint();

		// grab the middle fourth of the square and look at the colors inside
		for (int x = -(board.tileSize.width / 4) + centerPoint.width; x < board.tileSize.width / 4 + centerPoint.width; x++ )
			for (int y = (board.tileSize.height / 4) + centerPoint.height; y >  -(board.tileSize.height / 4) + centerPoint.height; y-- ) {
				// get mouse out of the way to the pixel reader
				board.r.mouseMove(board.offsetCorner.width + board.width, board.offsetCorner.height + board.length); 

				//scan in the colors
				colors.add(board.r.getPixelColor(x, y));				
			}
		
		return colors;
	}

	@Override

	public String toString() {
		return "Tile["+ xPos + "," + yPos + "]:: State: " +  getState() + ",Value: "+ getValue();
	}

	void determineInformation() {
		value = scanForValue();
	}
	
	void flagTile() {
		value = FLAG_VALUE;
	}
	
	String prettyString() {
		if (value == UNKNOWN_VALUE)
			return "?";
		if (value == BOMB_VALUE)
			return "B";
		if (value == FLAG_VALUE)
			return "F";
		if (value == EMPTY_VALUE)
			return "x";
		else return value + "";
	}
	
}
